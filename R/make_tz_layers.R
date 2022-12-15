#' Make kml and gpx layers for TimeZero
#' 
#' @param region Survey region as a character vector (passed to akfmaps::get_base_layers)
#' @export

make_tz_layers <- function(region) {
  
  # Retrieve shapefiles from akgfmaps package
  map_layers <- akgfmaps::get_base_layers(select.region = region, 
                                          set.crs = "EPSG:4326")
  
  if(!dir.exists(here::here("output", region))) {
    dir.create(here::here("output", region))
  }
  
  # Setup survey strata boundary layer
  map_layers$survey.strata |>
    dplyr::rename(Name = Stratum) |>
    sf::st_write(dsn = here::here("output", region, paste0("survey_strata.kml")),
                 driver = "KML", 
                 delete_dsn = TRUE)
  
  replace_kml_lines(kml_path = here::here("output", region, paste0("survey_strata.kml")),
                    line_pattern = "<Style>",
                    replacement = paste0("\t<Style><LineStyle><color>", 
                                         tz_pal(4)[4], 
                                         "</color></LineStyle><PolyStyle><color>0</color></PolyStyle></Style>"
                    )
  )
  
  # Setup survey grid marks layer --- Join with station allocation for the GOA/AI
  grid_centers <- sf::st_centroid(map_layers$survey.grid)
  grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)
  grid_centers <- as.data.frame(grid_centers) |>
    dplyr::select(-geometry)
  grid_centers$shape <- 3
  grid_centers$color <- 1
  grid_centers$description <- ""
  
  df_to_gpx(x = grid_centers, 
            name_col = "STATIONID",
            description_col = "description",
            lat_col = "latitude", 
            lon_col = "longitude", 
            color_col = "color", 
            shape_col = "shape", 
            gpx_format = "timezero", 
            gpx_file = here::here("output", region, "station_marks.gpx"))
  
  # Setup survey grid boundary layer --- NEED TO JOIN WITH GOOD/BAD GRID CELLS
  map_layers$survey.grid |>
    dplyr::rename(Name = STATIONID) |>
    sf::st_write(dsn = here::here("output", region, paste0("station_grid.kml")),
                 driver = "KML", 
                 delete_dsn = TRUE)
  
  replace_kml_lines(kml_path = here::here("output", region, paste0("station_grid.kml")),
                    line_pattern = "<Style>",
                    replacement = paste0("\t<Style><LineStyle><color>", 
                                         tz_pal(1)[1], 
                                         "</color></LineStyle><PolyStyle><color>0</color></PolyStyle></Style>"
                    )
  )
  
} 
