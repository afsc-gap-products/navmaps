#' Make trawlable/untrawlable shapefile and KML
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @param software_format Software to format output for.
#' @export

make_trawlable <- function(region, channel = NULL, software_format = "timezero") {
  
  .check_region(region)
  
  channel <- navmaps::get_connected(channel = channel)
  
  # Retrieve map layers from racebase
  message("make_trawlable: Retriving shapefiles using akgfmaps")
  map_layers <- akgfmaps::get_base_layers(select.region = region,
                                          set.crs = "EPSG:3338")
  
  # Create output directory
  dir.create(here::here("output", region, "shapefiles"),
             recursive = TRUE,
             showWarnings = FALSE)
  
  # Create output directory
  dir.create(here::here("output", region, "navigation"),
             recursive = TRUE,
             showWarnings = FALSE)
  
  # Query aigrid tables and join with survey grid shapefile
  if(region == "ai") {
    
    message("make_trawlable: Querying ai.aigrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from ai.aigrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(AIGRID_ID, STRATUM, geometry) |>
      dplyr::select(-STRATUM) |>
      dplyr::inner_join(trawlable, 
                        by = c("AIGRID_ID"))
  }
  
  if(region == "goa") {
    
    message("make_trawlable: Querying goa.goagrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select GOAGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from goa.goagrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(GOAGRID_ID, STRATUM, geometry) |>
      dplyr::select(-STRATUM) |>
      dplyr::inner_join(trawlable,
                        by = c("GOAGRID_ID"))
  }
  
  shp_path <- here::here("output", region, "shapefiles", paste0(region, "_trawlwable.shp"))
  
  message("make_trawlable: Writing trawlable/untrawlable shapefile to ", shp_path)
  
  # Write output to shapefile
  sf::st_write(obj = trawlable_grid, 
               dsn = shp_path, 
               append = FALSE)
                         
  message("make_trawlable: Converting to WGS84 and adding color/fill columns.")
  trawlable_grid <- sf::st_transform(trawlable_grid, crs = "EPSG:4326") |>
    sf::st_wrap_dateline()
  
  # Set plot colors
  
  if(software_format == "timezero") {
    trawlable_grid$color <- 1
    trawlable_grid$color[trawlable_grid$TRAWLABLE == 'Y'] <- 8
    trawlable_grid$color[trawlable_grid$TRAWLABLE == 'N'] <- 4
    trawlable_grid$fill <- 0
    trawlable_grid$description <- paste0("Trawlable? ", trawlable_grid$TRAWLABLE, "; Stratum: ", trawlable_grid$STRATUM)
    
    kml_path <- here::here("output", region, "navigation", paste0(region, "_trawlwable.kml"))
    
    message("make_trawlable: Writing trawlable/untrawlable kml to ", kml_path)
    sf_to_kml_polygon(x = trawlable_grid,
                      name_col = "STATIONID",
                      description_col = "description",
                      color_col = "color",
                      fill_col = "fill",
                      software_format = software_format,
                      file = kml_path)
  }
 
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
#' @param extra_col Names of columns containing data that should be included in the comments/description fields (e.g. "priority" if there's a sampling station priority field). Optional.
#' @param software_format Software to format output for.
#' @export

make_station_allocation <- function(allocation_df, region, survey_year = NULL, lon_col, lat_col, station_col, stratum_col, vessel_col, extra_col = NULL, software_format = "timezero") {
  
  .check_cols_exist(x = allocation_df, 
                    var_cols = c(lon_col, lat_col, station_col, stratum_col, vessel_col, extra_col), 
                    func_name = "make_station_allocation")
  
  .check_region(region)
  
  stopifnot("make_station_allocation: allocation_df must be a data.frame!" = all(class(allocation_df) == "data.frame"))
  
  if(is.null(survey_year)) {
    survey_year <- as.numeric(format(Sys.Date(), "%Y"))
    message("make_station_allocation: No surveyyear provided. Guessing survey_year is ", survey_year)
  }
  
  allocation_sf <- sf::st_as_sf(allocation_df, 
                                coords = c(lon_col, lat_col),
                                crs = "EPSG:4326")

  
  shp_path <- here::here("output", region, "shapefiles", paste0(region, "_", survey_year, "_station_allocation.shp"))
  
  
  message("make_station_allocation: Writing station allocation shapefile to ", shp_path)
  
  allocation_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_write(dsn = shp_path, 
                 append = FALSE)
  
  if(is.null(extra_col)) {
    allocation_sf$description <- paste0("Stratum: ", allocation_sf[[stratum_col]], "Vessel: ", allocation_sf[[vessel_col]])
  } else {
    
    allocation_sf$description <- paste0("Stratum: ", allocation_sf[[stratum_col]], "Vessel: ", allocation_sf[[vessel_col]])
    
    for(ii in 1:length(extra_col)) {
      allocation_sf$descripton <- paste0(allocation_sf$descripton, "; ", 
                                         extra_col[ii], ": ", 
                                         allocation_sf[[extra_col[ii]]])
    }
  }
  
  if(software_format == "timezero") {
    message("make_station_allocation: Adding color, shape and description columns.")
    allocation_sf$color <- as.numeric(factor(allocation_sf[[vessel_col]]))
    allocation_sf$shape <- as.numeric(factor(allocation_sf[[vessel_col]]))
    
    gpx_path <- here::here("output", region, "navigation", paste0(region, "_", survey_year, "_station_allocation.gpx"))
    
    message("make_station_allocation: Writing station allocation gpx file to ", gpx_path)
    
    sf_to_gpx_waypoints(x = allocation_sf, 
                        file = gpx_path,
                        name_col = station_col,
                        description_col = "description",
                        color_col = "color",
                        shape_col = "shape",
                        software_format = software_format)
  }
  
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
  
  if(length(raw_gps_paths) < 1) {
    stop("make_towpaths: No GPS rds files found in ", here::here("output", region, "gps"))
  } else {
    message("make_towpaths: ", length(raw_gps_paths), " raw_gps rds files found in ", raw_gps_paths)
  }
  
  midpoint_paths <- gsub(pattern = "raw_gps", replacement = "midpoint", x = raw_gps_paths)
  
  # Calculating midpoints
  
  # Function to closest point to the midpoint
  calc_midpoint <- function(coords, dist) {
    coords[which.min(abs(max(dist)/2 - dist))]
  }
  
  for(ii in 1:length(raw_gps_paths)) {
    
    if(overwrite_midpoint & file.exists(midpoint_paths[ii])) {
      message("make_towpaths: Skipping file that already exists: ", midpoint_paths[ii])
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
  
  # Convert to simple features and apply dateline wrap
  midpoint_sf <- sf::st_sf(midpoint_df) |>
    sf::st_wrap_dateline()
  
  # Write midpoints to shapefile ----
  midpoint_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_midpoint.shp"))
  
  message("make_towpaths: Writing trawlable/untrawlable shapefile to ", midpoint_shp_path)
  
  midpoint_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_write(dsn = midpoint_shp_path, 
                 append = FALSE)
  
  
  # Write start points to shapefile ----
  start_and_end <- readRDS(file = here::here("output", region, paste0(region, "_haul_start_end.rds"))) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")  
  
  start_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_towstart.shp"))
  
  message("make_towpaths: Writing tow starts shapefile to ", start_shp_path)
  
  start_and_end |>
    dplyr::filter(EVENT == "start") |>
    dplyr::select(-EVENT) |>
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |> # Rename PERFORMANCE_DESCRIPTION so PERFORMANCE and PERFORMANCE_DESCRIPTION have unique names when truncated to the maximum character length limit (7) for ESRI shapefile field names
    sf::st_write(dsn = start_shp_path, 
                 append = FALSE)
  
  # Write tow paths to shapefile ----
  towpath_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_towpath.shp"))
  
  message("make_towpaths: Writing towpath shapefile to ", towpath_shp_path)
  
  towpath_sf <- start_and_end |> 
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::group_by(VESSEL, CRUISE, HAUL, PERFORMANCE, PERFORMANCE_DESCRIPTION) |> 
    dplyr::summarize(do_union = FALSE) |> 
    sf::st_cast("LINESTRING")
  
  towpath_sf |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |> # Rename PERFORMANCE_DESCRIPTION so PERFORMANCE and PERFORMANCE_DESCRIPTION have unique names when truncated to the maximum character length limit (7) for ESRI shapefile field names
    sf::st_write(dsn = towpath_shp_path, 
                 append = FALSE)
  
  ###### FORMAT FILES AND WRITE FOR EACH SOFTWARE
  
  if(software_format == "timezero") {
    
    # View timezero color palette
    # show_col_nav(tz_pal(Inf, software_code = FALSE))
    
    sf_to_gpx_waypoints(x = start_and_end |>
                          dplyr::filter(EVENT == "start") |>
                          dplyr::mutate(name = paste0("Start ", CRUISE, "-", VESSEL),
                                        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                                        shape = factor(sign(PERFORMANCE)),
                                        color = tz_pal(11)[c(10, 8, 4)][as.numeric(sign(PERFORMANCE)) + 1]),
                        file = here::here("output", region, "navigation", paste0(region, "_towstart.gpx")),
                        name_col = "name",
                        description_col = "desc",
                        color_col = "color",
                        shape_col = "shape")
    
    
    sf_to_gpx_waypoints(x = midpoint_sf |>
                          dplyr::inner_join(as.data.frame(start_and_end) |>
                                              dplyr::filter(EVENT == "start") |>
                                              dplyr::select(-geometry, -HAULJOIN)) |>
                          dplyr::mutate(name = paste0("MID ", CRUISE, "-", VESSEL),
                                        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                                        shape = factor(sign(PERFORMANCE)),
                                        color = tz_pal(11)[c(10, 8, 4)][as.numeric(sign(PERFORMANCE)) + 1]),
                        file = here::here("output", region, "navigation", paste0(region, "_towmid.gpx")),
                        name_col = "name",
                        description_col = "desc",
                        color_col = "color",
                        shape_col = "shape")
    
    
    sf_to_kml_linestring(x = towpath_sf |>
                           dplyr::mutate(name = paste0("Start ", CRUISE, "-", VESSEL),
                                         desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION),
                                         color = tz_pal(11)[c(10, 8, 4)][as.numeric(sign(PERFORMANCE)) + 1]),
                         file = here::here("output", region, "navigation", paste0(region, "_towpath.kml")),
                         name_col = "name",
                         description_col = "desc",
                         color_col = "color")
  }
  
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
  
  # Calculate time elapsed
  elapsed_sec <- cumsum(c(0, difftime(sf_obj $DATE_TIME[2:nrow(sf_obj )], sf_obj $DATE_TIME[1:(nrow(sf_obj)-1)])))
  
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