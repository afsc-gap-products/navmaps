map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = 1:11, 
                            size = nrow(bathy_layer ), 
                            replace = TRUE)

sf_to_gpx_track(
  x = bathy_layer,
  name_col = "METERS",
  description_col = "LAST_UPDAT",
  color_col = "color",
  file = "test_lines.gpx")


sf::st_write(bathy_layer, "test.shp")

strata <- map_layers$survey.strata
strata$color <- 1:12
strata$fill <- 0

sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", "test_polygon.kml"))

sf_to_gpx_track(
  x = strata,
  name_col = "Stratum",
  description_col = "SURVEY",
  file = "test_polygon.gpx")

library(navmaps)

make_trawlable(region = "ai")
