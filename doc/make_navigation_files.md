# Writing simple features objects to marine navigation software files

*In development (1/7/2023)*

## POINTS to .gpx waypoint file

GPX point files are supported in OpenCPN and TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

grid_centers <- sf::st_centroid(map_layers$survey.grid)

grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)

# Assign TimeZero color for .gpx
grid_centers$shape = tz_pal(values = "yellow", type = "gpx")
grid_centers$color = 1

sf_to_gpx_waypoints(x = grid_centers, 
                    file = here::here("output", "test_marks.gpx"), 
                    name_col = "STATIONID",
                    description_col = "STATIONID",
                    color_col = "color", 
                    shape_col = "shape")
```

## sf POINTS to .kml points file

KML point files are supported in TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

grid_centers <- sf::st_centroid(map_layers$survey.grid)

grid_centers[c('longitude', 'latitude')] = sf::st_coordinates(grid_centers)

# Assign TimeZero color for .kml
grid_centers$shape = tz_pal(values = "yellow", type = "kml")
grid_centers$color = 1

sf_to_gpx_points(x = grid_centers, 
                 file = here::here("output", "test_marks.kml"), 
                 name_col = "STATIONID",
                 description_col = "STATIONID",
                 color_col = "color", 
                 shape_col = "shape")
```


## sf LINESTRINGS to .kml linestring file

KML linestring files are supported in TimeZero and ArcMap.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry

# Assign TimeZero color for kml
bathy_layer$color <- tz_pal(values = "blue", type = "kml")

sf_to_kml_linestring(x = bathy_layer,
                     name_col = "METERS",
                     description_col = "LAST_UPDAT",
                     color_col = "color",
                     file = here::here("output", "test_lines.kml"))
```


## sf POLYGON/MULTIPOLYGON to .kml polygon

KML polygon files are supported in TimeZero and ArcMap.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

strata <- map_layers$survey.strata
strata$color <- tz_pal(values = "blue", type = "kml")
strata$fill <- 0 # No fill

sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", "test_polygon.kml"))
```

## sf LINESTRING, MULTILINESTRING, POLYGON, or MULTIPOLYGON to .gpx track

These files work in OpenCPN and TimeZero.

```
library(navmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = 1:11, 
                            size = nrow(bathy_layer ), 
                            replace = TRUE)

sf_to_gpx_track(
  x = bathy_layer,
  name_col = "METERS",
  description_col = "LAST_UPDAT",
  file = here::here("output", "test_lines.gpx"))

strata <- map_layers$survey.strata

sf_to_gpx_track(
  x = strata,
  name_col = "Stratum",
  description_col = "SURVEY",
  file = here::here("output", "test_polygon.gpx"))
```