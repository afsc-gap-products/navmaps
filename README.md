# tzmaps
Convert spatial data to KML and GPX formats


## Convert sf POINTS to gpx waypoints
```
library(tzmaps)

map_layers <- akgfmaps::get_base_layers(region = "sebs")

grid_centers <- sf::st_centroid(map_layers$survey.grid)

grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)

grid_centers <- as.data.frame(grid_centers) |>
  dplyr::select(-geometry)
grid_centers$shape = 3
grid_centers$color = 1

sf_to_gpx_waypoints(x = grid_centers, 
          name_col = "STATIONID", 
          lat_col = "latitude", 
          lon_col = "longitude", 
          color_col = "color", 
          shape_col = "shape", 
          gpx_format = "timezero", 
          gpx_file = here::here("test_marks.gpx"))
```


## Convert sf LINESTRINGS to .kml linestring
```
library(tzmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry

bathy_layer$color <- sample(x = tz_pal(11), 
                  size = nrow(bathy_layer ), 
                  replace = TRUE)

sf_to_kml_linestring(x = bathy_layer,
                     name_col = "METERS",
                     description_col = "LAST_UPDAT",
                     color_col = "color",
                     kml_file = here::here("test_lines.kml"))
```


## Convert sf POLYGON/MULTIPOLYGON to .kml linestring
```
library(tzmaps)

map_layers <- akgfmaps::get_base_layers(region = "sebs")

strata <- map_layers$survey.strata
x$color <- tz_pal(5)[5]
x$fill <- 0

sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  kml_file = "test_polygon.kml")
```
