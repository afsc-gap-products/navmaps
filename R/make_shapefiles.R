#' Make trawlable/untrawlable shapefile
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param channel RODBC connection. Function will prompt for a connection if the user is not connected
#' @export

make_shp_trawlable <- function(region, channel = NULL) {
  
  channel <- navmaps::get_connected(channel = channel)
  
  # Retrieve map layers from racebase
  message("shp_trawlable: Retriving shapefiles using akgfmaps")
  map_layers <- akgfmaps::get_base_layers(select.region = region,
                                          set.crs = "EPSG:3338")
  
  # Create output directory
  dir.create(here::here("output", region, "shapefiles"),
             recursive = TRUE,
             showWarnings = FALSE)
  
  # Query aigrid tables and join with survey grid shapefile
  if(region == "ai") {
    
    message("shp_trawlable: Querying ai.aigrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select AIGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from ai.aigrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(-STRATUM) |>
      dplyr::inner_join(trawlable, 
                        by = c("AIGRID_ID"))
  }
  
  if(region == "goa") {
    
    message("shp_trawlable: Querying goa.goagrid_gis table for trawlable/untrawlable. Joining with survey.grid.")
    trawlable <- RODBC::sqlQuery(query = "select GOAGRID_ID, TRAWLABLE, STRATUM, STATIONID, CENTER_LAT, CENTER_LONG, SOUTH_LAT, EAST_LONG, WEST_LONG from goa.goagrid_gis",
                                 channel = channel)
    
    trawlable_grid <- map_layers$survey.grid |>
      dplyr::select(-STRATUM) |>
      dplyr::inner_join(map_layers$survey.grid,
                        by = c("GOAGRID_ID"))
  }
  
  out_path <- here::here("output", region, "shapefiles", paste0(region, "_trawlwable.shp"))
                         
                         message("shp_trawlable: Writing trawlable/untrawlable shapefile to ", out_path)
                         
                         # Write output to shapefile
                         sf::st_write(obj = trawlable_grid, 
                                      dsn = out_path, 
                                      append = FALSE)
}
