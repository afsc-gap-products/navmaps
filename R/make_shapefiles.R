#' Make trawlable/untrawlable shapefile and KML
#' 
#' This takes ~10 minutes to run for the AI.
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @export

make_trawlable <- function(region, channel = NULL, kml_format = "timezero") {
  
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
      dplyr::inner_join(map_layers$survey.grid,
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
  
  trawlable_grid$color <- 1
  trawlable_grid$color[trawlable_grid$TRAWLABLE == 'Y'] <- 8
  trawlable_grid$color[trawlable_grid$TRAWLABLE == 'N'] <- 4
  trawlable_grid$fill <- 0
  trawlable_grid$description <- paste0("Stratum ", trawlable_grid$STRATUM, "; Trawlable? ", trawlable_grid$TRAWLABLE)
  
  kml_path <- here::here("output", region, "navigation", paste0(region, "_trawlwable.kml"))
  
  message("make_trawlable: Writing trawlable/untrawlable kml to ", kml_path)
  sf_to_kml_polygon(x = trawlable_grid,
                    name_col = "STATIONID",
                    description_col = "description",
                    color_col = "color",
                    fill_col = "fill",
                    format = kml_format,
                    file = kml_path)
                         
}
