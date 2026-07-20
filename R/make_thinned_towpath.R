#' Make towpath, tow start, and midpoint from GPS rds files
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param overwrite_midpoint Should files be overwritten?
#' @param software_format Software format as a character vector.
#' @param filename_suffix Optional character vector to append to file name.
#' @export
#' @import purrr tidyr dplyr rmapshaper lubridate

make_towpaths <- function(region, overwrite_midpoint = FALSE, software_format = "timezero", filename_suffix = NULL) {
  
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
  
  start_end_wide <- 
    start_and_end |>
    as.data.frame() |>
    dplyr::select(VESSEL, CRUISE, HAUL, DATE_TIME, EVENT) |>
    dplyr::mutate(DATE_TIME = lubridate::force_tz(DATE_TIME, "UTC")) |>
    tidyr::pivot_wider(
      id_cols = c("VESSEL", "CRUISE", "HAUL"),
      values_from = "DATE_TIME",
      names_from = "EVENT"
    )
  
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
    safe_st_write(dsn = start_shp_path, 
                  append = FALSE)
  # Midpoints ----
  midpoint_paths <- gsub(pattern = "raw_gps", replacement = "midpoint", x = raw_gps_paths)
  
  # Thinned towpaths
  thinned_towpath_path <- gsub(pattern = "raw_gps", replacement = "full_towpath", x = raw_gps_paths)
  
  # Calculating midpoints
  
  # Function to closest point to the midpoint
  calc_midpoint <- function(coords, dist) {
    coords[which.min(abs(max(dist)/2 - dist))]
  }
  
  for(ii in 1:length(raw_gps_paths)) {
    
    if(!overwrite_midpoint & file.exists(midpoint_paths[ii])) {
      message("make_towpaths: overwrite_midpoint set to FALSE. Skipping file that already exists: ", midpoint_paths[ii])
      
      # Smooth latitude and longitude, calculate elapsed time, and distance 
      temp_gps <- readRDS(raw_gps_paths[ii]) |>
        dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
        dplyr::arrange(DATE_TIME) |>
        tidyr::nest(df = c(LATITUDE, LONGITUDE, DATE_TIME)) |>
        dplyr::mutate(df = purrr::map(df, navmaps::smooth_lat_lon_dist, spar = 0.9)) |>
        tidyr::unnest(cols = df)
      
    } else{
      message("make_towpaths: Processing ", raw_gps_paths[ii])
      
      # Find the point closest to the midpoint
      midpoint <-  temp_gps |>
        dplyr::group_by(VESSEL, CRUISE, HAUL) |>
        dplyr::summarise(geometry = calc_midpoint(geometry, dist = DISTANCE_M))
      
      # Write midpoint data to rds file
      message("make_towpaths: Writing midpoint data to ", midpoint_paths[ii])
      saveRDS(midpoint, file = midpoint_paths[ii])

    }
    
    # Create thinned towpaths for all years after 2005 - GPS stabilized
    if(all(lubridate::year(temp_gps$DATE_TIME) > 2005)) {
      
      thinned_towpath <- 
        readRDS(raw_gps_paths[ii]) |>
        dplyr::inner_join(start_end_wide) |>
        dplyr::filter(DATE_TIME > start & DATE_TIME < end) |>
        dplyr::arrange(DATE_TIME) |>
        dplyr::mutate(date = as.POSIXct(as.Date(DATE_TIME), tz = "America/Anchorage")) |>
        sf::st_as_sf(crs = "WGS84", coords = c("LONGITUDE", "LATITUDE")) |>
        dplyr::group_by(VESSEL, CRUISE, HAUL, date) |>
        dplyr::summarise(do_union = FALSE) |>
        sf::st_cast(to = "LINESTRING") |>
        rmapshaper::ms_simplify(keep = 0.2)
      
      # Write thinned towpath data to rds file
      message("make_towpaths: Writing thinned towpath data to ", thinned_towpath_path[ii])
      saveRDS(thinned_towpath, file = thinned_towpath_path[ii])
    }
    
  }
  
  # Load midpoint and haul data
  midpoint_paths <- list.files(here::here("output", region, "gps"), pattern = "midpoint", full.names = TRUE)
  
  midpoint_df <- 
    lapply(X = midpoint_paths, FUN = readRDS)
    
  midpoint_df <- do.call(what = bind_rows, args = midpoint_df)
  
  # Convert to simple features, apply dateline wrap, and add start time and performance
  midpoint_sf <- sf::st_sf(midpoint_df) |>
    sf::st_wrap_dateline() |>
    dplyr::inner_join(as.data.frame(start_sf) |>
                        dplyr::select(VESSEL, CRUISE, HAUL, START_TIME, PERFORMANCE, PERFORMANCE_DESCRIPTION, START_TIME, BOTTOM_DEPTH), 
                      by = c("VESSEL", "CRUISE", "HAUL")) |>
    sf::st_as_sf(crs = "EPSG:4326")
  
  # Write midpoints to shapefile ----
  midpoint_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_midpoint", filename_suffix, ".shp"))
  
  message("make_towpaths: Writing trawlable/untrawlable shapefile to ", midpoint_shp_path)
  
  midpoint_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |>
    safe_st_write(dsn = midpoint_shp_path, 
                  append = FALSE)
  
  # Write tow simplified towpaths to shapefile ----
  towpath_shp_path <- here::here("output", region, "shapefiles", paste0(region, "_towpath", filename_suffix, ".shp"))
  
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
  obj <- towpath_sf |>
    sf::st_transform(crs = "EPSG:3338") |>
    dplyr::rename(PERFDES = PERFORMANCE_DESCRIPTION) |>
    safe_st_write(dsn = towpath_shp_path,
                  append = FALSE)
  
  thinned_towpath_paths <- 
    list.files(path = dirname(thinned_towpath_path[1]), pattern = "full_towpath", full.names = TRUE)
  
  thinned_towpath_sf <- 
    lapply(X = thinned_towpath_paths, FUN = readRDS) |>
    do.call(what = bind_rows) |>
    dplyr::inner_join(
      as.data.frame(start_sf) |>
        dplyr::select(VESSEL, CRUISE, HAUL, PERFORMANCE, PERFORMANCE_DESCRIPTION),
      by = c("VESSEL", "CRUISE", "HAUL")
    )
  
  # Add symbol, color, description and name fields for nav software. 
  # Specify required column names to sf_to_nav: file, name_col, description_col, color_col, shape_col, time_col, extra_cols, and software_format
  
  if(software_format == "timezero") {
  
    start_sf <- start_sf |> 
      dplyr::mutate(
        name = floor(CRUISE/100),
        desc = paste0("Vessel: ", VESSEL, ", Haul: ", HAUL, " (", PERFORMANCE, "-", PERFORMANCE_DESCRIPTION, ")")
      )
    
    midpoint_sf <- midpoint_sf |> 
      dplyr::mutate(
        name = floor(CRUISE/100),
        desc = paste0("Vessel: ", VESSEL, ", Haul: ", HAUL, " (", PERFORMANCE, "-", PERFORMANCE_DESCRIPTION, ")")
      )
    
    towpath_sf <- towpath_sf |>
      dplyr::mutate(
        name = "",
        desc = paste0("Yr: ", floor(CRUISE/100), ", Vessel: ", VESSEL, ", Haul: ", HAUL, " (", PERFORMANCE, "-", PERFORMANCE_DESCRIPTION, ")")
      )
    
    thinned_towpath_sf <- thinned_towpath_sf |>
      dplyr::mutate(
        name = "",
        desc = paste0("Yr: ", floor(CRUISE/100), ", Vessel: ", VESSEL, ", Haul: ", HAUL, " (", PERFORMANCE, "-", PERFORMANCE_DESCRIPTION, ")")
      )
    
  } else {
    
    start_sf <- start_sf |> 
      dplyr::mutate(
        name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION)
      )
    
    midpoint_sf <- midpoint_sf |> 
      dplyr::mutate(
        name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION)
      )
    
    towpath_sf <- towpath_sf |>
      dplyr::mutate(
        name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION)
      )
    
    thinned_towpath_sf <- thinned_towpath_sf |>
      dplyr::mutate(
        name = paste0(floor(CRUISE/100), "/", VESSEL, "/", HAUL),
        desc = paste0(PERFORMANCE, ": ", PERFORMANCE_DESCRIPTION)
      )
    
  }
  
  start_sf <- start_sf |>
    dplyr::mutate(
      shape = navmaps_sym_pal(values = c("asterisk", "diamond", "diamond"), 
                              software_format = software_format,
                              file_type = file_type_marks)[factor(sign(PERFORMANCE)+2)],
      depth = BOTTOM_DEPTH,
      temperature = GEAR_TEMPERATURE,
      color = navmaps_pal(values = c("red", "lightgreen", "purple"),
                          file_type = file_type_marks,
                          software_format = software_format)[as.numeric(sign(PERFORMANCE)) + 2]
    )
  
  sf_to_nav_file(
    x = start_sf,
    file = here::here("output", region, "navigation", software_format, paste0(region, "_towstart", filename_suffix, ".", file_type_marks)),
    name_col = "name",
    description_col = "desc",
    color_col = "color",
    shape_col = "shape",
    time_col = "START_TIME",
    extra_cols = c("temperature", "depth"),
    software_format = software_format
  )
  
  sf_to_nav_file(
    x = dplyr::filter(start_sf, CRUISE < 200500),
    file = here::here("output", region, "navigation", software_format, paste0(region, "_towstart_thru2005", filename_suffix, ".", file_type_marks)),
    name_col = "name",
    description_col = "desc",
    color_col = "color",
    shape_col = "shape",
    time_col = "START_TIME",
    extra_cols = c("temperature", "depth"),
    software_format = software_format
  )
  
  sf_to_nav_file(
    x = dplyr::filter(start_sf, CRUISE > 200500),
    file = here::here("output", region, "navigation", software_format, paste0(region, "_towstart_after2005", filename_suffix, ".", file_type_marks)),
    name_col = "name",
    description_col = "desc",
    color_col = "color",
    shape_col = "shape",
    time_col = "START_TIME",
    extra_cols = c("temperature", "depth"),
    software_format = software_format
  )
  
  midpoint_sf |>
    dplyr::mutate(
      shape = navmaps_sym_pal(values = c("asterisk", "diamond", "diamond"), 
                              software_format = software_format,
                              file_type = file_type_marks)[factor(sign(PERFORMANCE)+2)],
      depth = BOTTOM_DEPTH,
      color = navmaps_pal(values = c("red", "lightgreen", "purple"), 
                          file_type = file_type_marks,
                          software_format = software_format)[as.numeric(sign(PERFORMANCE)) + 2]
    ) |>
    sf_to_nav_file(
      file = here::here("output", region, "navigation", software_format, paste0(region, "_towmid", filename_suffix, ".", file_type_marks)),
      name_col = "name",
      description_col = "desc",
      color_col = "color",
      shape_col = "shape",
      time_col = "START_TIME",
      extra_cols = "depth",
      software_format = software_format
    )
  
  print(here::here("output", region, "navigation", software_format, paste0(region, "_towpath", filename_suffix, ".", file_type_lines)))
  
  towpath_sf$color <- 
    navmaps_pal(
      values = c("red", "lightgreen", "purple"), 
      file_type = file_type_lines,
      software_format = software_format)[as.numeric(sign(towpath_sf$PERFORMANCE)) + 2]
  
  sf_to_nav_file(x = towpath_sf,
                 file = here::here("output", region, "navigation", software_format, paste0(region, "_towpath", filename_suffix, ".", file_type_lines)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 time_col = "START_TIME",
                 software_format = software_format)
  
  sf_to_nav_file(x = dplyr::filter(towpath_sf, CRUISE < 200500),
                 file = here::here("output", region, "navigation", software_format, paste0(region, "_towpaths_thru2005", filename_suffix, ".", file_type_lines)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 time_col = "START_TIME",
                 software_format = software_format)
  
  thinned_towpath_sf$color <- 
    navmaps_pal(values = c("red", "lightgreen", "purple"), 
                file_type = file_type_lines,
                software_format = software_format)[as.numeric(sign(thinned_towpath_sf$PERFORMANCE)) + 2]
  
  sf_to_nav_file(x = thinned_towpath_sf,
                 file = here::here("output", region, "navigation", software_format, paste0(region, "_towpaths_after2005", filename_suffix, ".", file_type_lines)),
                 name_col = "name",
                 description_col = "desc",
                 color_col = "color",
                 time_col = "date",
                 software_format = software_format)
  
}