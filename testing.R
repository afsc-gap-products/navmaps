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