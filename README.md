# tzmaps
Convert spatial data to KML and GPX formats for display in TimeZero


## Convert points to gpx waypoints
```
library(tzmaps)

make_tz_layers(region = "sebs")

grid_centers = sf::st_centroid(map_layers$survey.grid)
grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)
grid_centers = as.data.frame(grid_centers) |>
  dplyr::select(-geometry)
grid_centers$shape = 3
grid_centers$color = 1

sf_to_gpx(x = grid_centers, 
          name_col = "STATIONID", 
          lat_col = "latitude", 
          lon_col = "longitude", 
          color_col = "color", 
          shape_col = "shape", 
          gpx_format = "timezero", 
          gpx_file = here::here("test_survey_marks.gpx"))
```


## Convert linestrings to .kml linestring
```
map_layers <- akgfmaps::get_base_layers(select.region = "sebs")
bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = tz_pal(11), 
                  size = nrow(bathy_layer ), 
                  replace = TRUE)

sf_to_kml_linestring(x = bathy_layer,
                     name_col = "METERS",
                     description_col = "LAST_UPDAT",
                     color_col = "color",
                     kml_file = "test_lines.kml")
```