#' Make trawlable/untrawlable shapefile and KML
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
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
                     format = software_format,
                     file = kml_path)
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
#' @param file_format "gpx" or "kml"
#' @param software_format Software to format output for.
#' @export

make_station_allocation <- function(allocation_df, region, survey_year = NULL, lon_col, lat_col, station_col, stratum_col, vessel_col, extra_col = NULL, file_format = "gpx", software_format = "timezero") {
  
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
  
  message("make_station_allocation: Adding color, shape and description columns.")
  allocation_sf$color <- as.numeric(factor(allocation_sf[[vessel_col]]))
  allocation_sf$shape <- as.numeric(factor(allocation_sf[[vessel_col]]))
  
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
  
  if(file_format == "gpx") {
    gpx_path <- here::here("output", region, "navigation", paste0(region, "_", survey_year, "_station_allocation.gpx"))
    
    message("make_station_allocation: Writing station allocation gpx file to ", gpx_path)
    
    sf_to_gpx_waypoints(x = allocation_sf, 
                        file = gpx_path,
                        name_col = station_col,
                        description_col = "description",
                        color_col = "color",
                        shape_col = "shape",
                        format = software_format)
  }
  
}
