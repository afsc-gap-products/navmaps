#' Make trawlable/untrawlable shapefile and KML
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @param software_format Software to format output for.
#' @export

make_trawlable <- function(region, channel = NULL, software_format = "timezero") {
  
  .check_region(region)
  
  .check_software(software_format)
  
  file_type_grid <- set_file_type(software_format, marks = FALSE)
  file_type_mark <- set_file_type(software_format, marks = TRUE)
  
  channel <- navmaps::get_connected(channel = channel)
  
  # Retrieve map layers from racebase
  message("make_trawlable: Retriving shapefiles using akgfmaps")
  map_layers <- akgfmaps::get_base_layers(select.region = region,
                                          set.crs = "EPSG:3338")
  
  # Query AIGRID or GOAGRID table and join with survey grid shapefile
  if(region == "ai") {
    
    message("make_trawlable: Querying ai.aigrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from ai.aigrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(AIGRID_ID, STRATUM, geometry) |>
      dplyr::inner_join(trawlable, 
                        by = c("AIGRID_ID", "STRATUM"))
    
  }
  
  if(region == "goa") {
    
    message("make_trawlable: Querying goa.goagrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select GOAGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from goa.goagrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(GOAGRID_ID, STRATUM, geometry) |>
      dplyr::inner_join(trawlable,
                        by = c("GOAGRID_ID", "STRATUM")) |>
      dplyr::filter(!is.na(STATIONID))
  }
  
  shp_path_grid <- here::here("output", region, "shapefiles", paste0(region, "_trawlwable_grid.shp"))
  
  .check_output_path(shp_path_grid)
  message("make_trawlable: Writing trawlable/untrawlable shapefile to ", shp_path_grid)
  
  # Write output to shapefile
  sf::st_write(obj = trawlable_grid, 
               dsn = shp_path_grid, 
               append = FALSE)
                         
  message("make_trawlable: Converting to WGS84 and adding color/fill columns.")
  trawlable_grid <- sf::st_transform(trawlable_grid, crs = "EPSG:4326") |>
    sf::st_wrap_dateline()
  
  # Set plot colors
    trawlable_grid$color <- navmaps_pal(values = "cyan", 
                                        software_format = software_format, 
                                        file_type = file_type_grid)
    trawlable_grid$color[trawlable_grid$TRAWLABLE == 'Y'] <- navmaps_pal(values = "lightgreen", 
                                                                         software_format = software_format, 
                                                                         file_type = file_type_grid)
    trawlable_grid$color[trawlable_grid$TRAWLABLE == 'N'] <- navmaps_pal(values = "red", 
                                                                         software_format = software_format, 
                                                                         file_type = file_type_grid)
    trawlable_grid$fill <- 0
    trawlable_grid$description <- paste0("Trawlable? ", trawlable_grid$TRAWLABLE, "; Stratum: ", trawlable_grid$STRATUM)
    
    grid_path <- here::here("output", region, "navigation", software_format, paste0(region, "_trawlwable_grid.", file_type_grid))
    
    mark_path <- here::here("output", region, "navigation", software_format, paste0(region, "_trawlwable_mark.", file_type_mark))
    
    .check_output_path(grid_path)
    
    .check_output_path(mark_path)
    
    trawlable_mark <- sf::st_centroid(trawlable_grid)

    message("make_trawlable: Writing trawlable/untrawlable grid file to ", grid_path)
    sf_to_nav_file(x = trawlable_grid,
                   file = grid_path,
                   name_col = "STATIONID",
                   description_col = "description",
                   color_col = "color",
                   fill_col = "fill",
                   software_format = software_format)
    
    message("make_trawlable: Writing trawlable/untrawlable mark file to ", mark_path)
    
    trawlable_mark$shape <- navmaps_sym_pal(values = "circle1", 
                                            software_format = software_format,
                                            file_type = file_type_mark)
    
    sf_to_nav_file(x = trawlable_mark,
                   file = mark_path,
                   name_col = "STATIONID",
                   description_col = "description",
                   color_col = "color",
                   shape_col = "shape",
                   software_format = software_format)
 
}



#' Make station allocation shapefile and navigation plots
#' 
#' @param allocation_df Data.frame containing allocated station latitude, longitude, stratum, station ID, vessel ID, and optional additional description columns (e.g. station priority).
#' @param survey_year Year for the survey. Optional.
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param lon_col Name of the longitude column as a character vector. Required.
#' @param lat_col Name of the latitude column as a character vector. Required.
#' @param station_col Name of the station ID column as a character vector. Required.
#' @param stratum_col Name of the stratum column as a character vector. Required.
#' @param vessel_col Name of the vessel ID column as a character vector Required.
#' @param extra_cols Names of columns containing data that should be included in the comments/description fields (e.g. "priority" if there's a sampling station priority field). Optional.
#' @param software_format Software to format output for.
#' @export

make_station_allocation <- function(allocation_df, region, lon_col, lat_col, station_col, stratum_col, vessel_col, extra_cols = NULL, software_format = "timezero") {
  
  .check_cols_exist(x = allocation_df, 
                    var_cols = c(lon_col, lat_col, station_col, stratum_col, vessel_col, extra_cols))
  
  .check_region(region)
  
  file_type <- set_file_type(software_format, marks = TRUE)
  
  stopifnot("make_station_allocation: allocation_df must be a data.frame!" = all(class(allocation_df) == "data.frame"))
  
  allocation_sf <- sf::st_as_sf(allocation_df, 
                                coords = c(lon_col, lat_col),
                                crs = "EPSG:4326")

  
  shp_path <- here::here("output", region, "shapefiles", paste0(region, "_station_allocation.shp"))
  
  .check_output_path(shp_path)
  
  message("make_station_allocation: Writing station allocation shapefile to ", shp_path)
  
  allocation_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_write(dsn = shp_path, 
                 append = FALSE)
  
  if(is.null(extra_cols)) {
    allocation_sf$description <- paste0("Stratum: ", allocation_sf[[stratum_col]], ", Vessel: ", allocation_sf[[vessel_col]])
  } else {
    
    allocation_sf$description <- paste0("Stratum: ", allocation_sf[[stratum_col]], ", Vessel: ", allocation_sf[[vessel_col]])
    
    for(ii in 1:length(extra_cols)) {
      allocation_sf$descripton <- paste0(allocation_sf$descripton, "; ", 
                                         extra_cols[ii], ": ", 
                                         allocation_sf[[extra_cols[ii]]])
    }
  }
  
    message("make_station_allocation: Adding color, shape and description columns.")
    
    allocation_sf$color <- navmaps_pal(values = c("yellow", "magenta"), 
                                           software_format = software_format,
                                           file_type = file_type)[as.numeric(factor(allocation_sf[[vessel_col]]))]
    
    allocation_sf$shape <- navmaps_sym_pal(values = c("circle1", "triangle1"), 
                                           software_format = software_format,
                                           file_type = file_type)[as.numeric(factor(allocation_sf[[vessel_col]]))]
    
    allocation_sf$time <- Sys.time()
    
    # print(head(allocation_sf))
    
    fpath <- here::here("output", region, "navigation", software_format, paste0(region, "_station_allocation.", file_type))
    
    message("make_station_allocation: Writing station allocation file to ", fpath)
    
    sf_to_nav_file(x = allocation_sf, 
                   file = fpath,
                   name_col = station_col,
                   description_col = "description",
                   color_col = "color",
                   shape_col = "shape",
                   time_col = "time",
                   extra_cols = extra_cols,
                   software_format = software_format)
  
}



#' Make towpath, tow start, and midpoint from GPS rds files
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param overwrite_midpoint Should files be overwritten?
#' @param software_format Software format as a character vector.
#' @export

make_towpaths <- function(region, overwrite_midpoint = FALSE, software_format = "timezero") {
  
  .check_region(x = region)
  
  raw_gps_paths <- list.files(here::here("output", region, "gps"), pattern = "raw_gps", full.names = TRUE)
  
  file_type_marks <- set_file_type(software_format = software_format, marks = TRUE)
  file_type_lines <- set_file_type(software_format = software_format, marks = FALSE)
  
  if(length(raw_gps_paths) < 1) {
    stop("make_towpaths: No GPS rds files found in ", here::here("output", region, "gps"))
  } else {
    message("make_towpaths: ", length(raw_gps_paths), 
            " raw_gps rds files found in ", 
            dirname(raw_gps_paths)[1])
  }
  
  # Write start points to shapefile ----
  start_and_end <- readRDS(file = here::here("output", region, paste0(region, "_haul_start_end.rds"))) |>
    dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")  
  
  start_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_towstart.shp"))
  
  .check_output_path(start_shp_path)
  
  message("make_towpaths: Writing tow starts shapefile to ", start_shp_path)
  
  # Select only start points and transform CRS for plotting
  start_sf <- start_and_end |>
    dplyr::filter(EVENT == "start") |>
    dplyr::select(-EVENT) |>
    dplyr::rename(START_TIME = DATE_TIME) |>
    sf::st_transform(crs = "EPSG:4326")
  
  # Rename PERFORMANCE_DESCRIPTION so PERFORMANCE and PERFORMANCE_DESCRIPTION have unique names when truncated to the maximum character length limit (7) for ESRI shapefile field names
  start_sf |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |>
    sf::st_write(dsn = start_shp_path, 
                 append = FALSE)
  
  # Midpoints ----
  midpoint_paths <- gsub(pattern = "raw_gps", replacement = "midpoint", x = raw_gps_paths)
  
  # Calculating midpoints
  
  # Function to closest point to the midpoint
  calc_midpoint <- function(coords, dist) {
    coords[which.min(abs(max(dist)/2 - dist))]
  }
  
  for(ii in 1:length(raw_gps_paths)) {
    
    if(!overwrite_midpoint & file.exists(midpoint_paths[ii])) {
      message("make_towpaths: overwrite_midpoint set to FALSE. Skipping file that already exists: ", midpoint_paths[ii])
      next
    } else{
      message("make_towpaths: Processing ", raw_gps_paths[ii])
      
      # Smooth latitude and longitude, calculate elapsed time, and distance 
      temp_gps <- readRDS(raw_gps_paths[ii]) |>
        dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
        dplyr::arrange(DATE_TIME) |>
        tidyr::nest(df = c(LATITUDE, LONGITUDE, DATE_TIME)) |>
        dplyr::mutate(df = purrr::map(df, navmaps::smooth_lat_lon_dist, spar = 0.9)) |>
        tidyr::unnest(cols = df)
      
      # Find the point closest to the midpoint
      midpoint <-  temp_gps |>
        dplyr::group_by(VESSEL, CRUISE, HAUL) |>
        dplyr::summarise(geometry = calc_midpoint(geometry, dist = DISTANCE_M))
      
      # Write midpoint data to rds file
      message("make_towpaths: Writing midpoint data to ", midpoint_paths[ii])
      saveRDS(midpoint, file = midpoint_paths[ii])
    }
  }
  
  # Load midpoint and haul data
  
  midpoint_paths <- list.files(here::here("output", region, "gps"), pattern = "midpoint", full.names = TRUE)
  
  midpoint_df <- readRDS(file = midpoint_paths[1])
  
  for(jj in 2:length(midpoint_paths)) {
    
    midpoint_df <- dplyr::bind_rows(midpoint_df,
                                    readRDS(file = midpoint_paths[jj]))
  }
  
  # Convert to simple features, apply dateline wrap, and add start time and performance
  midpoint_sf <- sf::st_sf(midpoint_df) |>
    sf::st_wrap_dateline() |>
    dplyr::inner_join(as.data.frame(start_sf) |>
                        dplyr::select(VESSEL, CRUISE, HAUL, START_TIME, PERFORMANCE, PERFORMANCE_DESCRIPTION, START_TIME, BOTTOM_DEPTH), 
                      by = c("VESSEL", "CRUISE", "HAUL")) |>
    sf::st_as_sf(crs = "EPSG:4326")
  
  # Write midpoints to shapefile ----
  midpoint_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_midpoint.shp"))
  
  message("make_towpaths: Writing trawlable/untrawlable shapefile to ", midpoint_shp_path)
  
  midpoint_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |> 
    sf::st_write(dsn = midpoint_shp_path, 
                 append = FALSE)
  
  # Write tow paths to shapefile ----
  towpath_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_towpath.shp"))
  
  .check_output_path(towpath_shp_path)
  
  message("make_towpaths: Writing towpath shapefile to ", towpath_shp_path)
  
  towpath_sf <- start_and_end |> 
    dplyr::group_by(VESSEL, CRUISE, HAUL, PERFORMANCE, PERFORMANCE_DESCRIPTION, BOTTOM_DEPTH) |> 
    dplyr::summarize(do_union = FALSE) |> 
    sf::st_cast("LINESTRING") |>
    dplyr::inner_join(as.data.frame(start_sf) |>
                        dplyr::select(VESSEL, CRUISE, HAUL, START_TIME), 
                      by = c("VESSEL", "CRUISE", "HAUL"))
  
  # Rename PERFORMANCE_DESCRIPTION so PERFORMANCE and PERFORMANCE_DESCRIPTION have unique names when truncated to the maximum character length limit (7) for ESRI shapefile field names
  towpath_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |> 
    sf::st_write(dsn = towpath_shp_path, 
                 append = FALSE)
  
  # Add symbol, color, description and name fields for nav software. 
  # Specify required column names to sf_to_nav: file, name_col, description_col, color_col, shape_col, time_col, extra_cols, and software_format
  midpoint_sf |>
    dplyr::mutate(name = paste0(floor(CRUISE/100), " - ", VESSEL),
                  desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                  shape = navmaps_sym_pal(values = c("asterisk", "diamond", "triangle1"), 
                                           software_format = software_format,
                                          file_type = file_type_marks)[factor(sign(PERFORMANCE)+2)],
                  color = navmaps_pal(values = c("red", "lightgreen", "purple"), 
                                      file_type = file_type_marks,
                                      software_format = software_format)[as.numeric(sign(PERFORMANCE)) + 2]) |>
    sf_to_nav_file(file = here::here("output", region, "navigation", software_format, paste0(region, "_towmid.", file_type_marks)),
                   name_col = "name",
                   description_col = "desc",
                   color_col = "color",
                   shape_col = "shape",
                   time_col = "START_TIME",
                   extra_cols = c("PERFORMANCE", "PERFORMANCE_DESCRIPTION", "BOTTOM_DEPTH"),
                   software_format = software_format)
  
  start_sf |>
    dplyr::mutate(name = paste0(floor(CRUISE/100), " - ", VESSEL),
                  desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                  shape = navmaps_sym_pal(values = c("asterisk", "diamond", "triangle1"), 
                                          software_format = software_format,
                                          file_type = file_type_marks)[factor(sign(PERFORMANCE)+2)],
                  color = navmaps_pal(values = c("red", "lightgreen", "purple"),
                                      file_type = file_type_marks,
                                      software_format = software_format)[as.numeric(sign(PERFORMANCE)) + 2]) |>                                   
  sf_to_nav_file(file = here::here("output", region, "navigation", software_format, paste0(region, "_towstart.", file_type_marks)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 shape_col = "shape",
                 time_col = "START_TIME",
                 extra_cols = c("PERFORMANCE", "PERFORMANCE_DESCRIPTION", "BOTTOM_DEPTH"),
                 software_format = software_format)
  
  print(here::here("output", region, "navigation", software_format, paste0(region, "_towpath.", file_type_lines)))
  
   towpath_sf |>
    dplyr::mutate(name = paste0(floor(CRUISE/100), " - ", VESSEL),
                  desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                  color = navmaps_pal(values = c("red", "lightgreen", "purple"), 
                                      file_type = file_type_lines,
                                      software_format = software_format)[as.numeric(sign(PERFORMANCE)) + 2]) |>
    sf_to_nav_file(file = here::here("output", region, "navigation", software_format, paste0(region, "_towpath.", file_type_lines)),
                   name_col = "name",
                   description_col = "desc",
                   color_col = "color",
                   time_col = "START_TIME",
                   extra_cols = c("PERFORMANCE", "PERFORMANCE_DESCRIPTION", "BOTTOM_DEPTH"),
                   software_format = software_format)
}



#' Function to calculate smooth latitude and longitude and calculate distance and time elapsed
#' 
#' Internal function called in make_towpaths() to smooth tow path and calculate midpoints. Multiple steps are combined in this function to save computing time converting between coordinate reference systems (geodetic WGS84 to projected UTM and back to WGS84).
#' 
#' @param df data.frame of GPS data containing LONGITUDE, LATITUDE, and DATE_TIME columns
#' @param spar Smoothing parameter argument passed to smooth.spline()
#' @keywords internal
#' @export

smooth_lat_lon_dist <- function(df, spar = NULL) {
  
  # Detect UTM zone
  utm_zone <- navmaps::longitude_to_utm_zone(df$LONGITUDE[1])
  utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # Convert object to UTM for distance calculations
  sf_obj <- sf::st_as_sf(df, 
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "EPSG:4326") |>
    sf::st_transform(crs = utm_crs)
  
  coords <- sf::st_coordinates(sf_obj)
  
  # Calculate time elapsed for each ping
  elapsed_sec <- cumsum(c(0, difftime(sf_obj $DATE_TIME[2:nrow(sf_obj )], sf_obj $DATE_TIME[1:(nrow(sf_obj)-1)])))
  
  # Handle case where smoothing isn't possible
  if(length(unique(elapsed_sec)) >= 4) {
  
  # Correct GPS error using splines on latitude and longitude with time elapsed a predictor
  lon_spline <- smooth.spline(x = elapsed_sec, y = coords[, 'X'], spar = spar)
  lat_spline <- smooth.spline(x = elapsed_sec, y = coords[, 'Y'], spar = spar)
  
  # Add predictions and elapsed time back to the data frame
  smoothed_df <- tidyr::tibble(LONGITUDE = predict(lon_spline)$y,
                               LATITUDE = predict(lat_spline)$y,
                               DATE_TIME = df$DATE_TIME,
                               ELAPSED_SEC = elapsed_sec) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 crs = utm_crs)
  } else {
    smoothed_df <- tidyr::tibble(LONGITUDE = coords[, 'X'],
                  LATITUDE = coords[, 'Y'],
                  DATE_TIME = df$DATE_TIME,
                  ELAPSED_SEC = elapsed_sec) |>
      sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                   crs = utm_crs)
  }
  
  # Calculate distance between points in meters
  dist_sf <- smoothed_df |>
    dplyr::mutate(dist = sf::st_distance(geometry, dplyr::lag(geometry), by_element = TRUE))
  dist_sf$dist[1] <- 0
  
  # Cumulative distance traveled in meters
  smoothed_df$DISTANCE_M <- cumsum(as.numeric(dist_sf$dist))
  
  # Convert back to WGS84
  smoothed_df <- smoothed_df |>
    sf::st_transform(crs = "EPSG:4326") |>
    dplyr::mutate(LONGITUDE = sf::st_coordinates(geometry)[, 'X'],
                  LATITUDE = sf::st_coordinates(geometry)[, 'Y']) |>
    tibble::as_tibble()
  
  return(smoothed_df)
  
}