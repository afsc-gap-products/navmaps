library(tzmaps)

# Step #1 convert the object to sf.
# Step #2 Format output

# Convert points to gpx waypoints
make_tz_layers(region = "sebs")
map_layers <- akgfmaps::get_base_layers(select.region = "sebs")

grid_centers = sf::st_centroid(map_layers$survey.grid)
grid_centers[c('longitude', 'latitude')] <- sf::st_coordinates(grid_centers)

grid_centers$shape <- 3
grid_centers$color <- 1

sf_to_gpx_waypoints(x = grid_centers,
                    gpx_file = here::here("test_survey_marks.gpx"), 
                    name_col = "STATIONID",
                    description_col = "STATIONID",
                    color_col = "color", 
                    shape_col = "shape", 
                    gpx_format = "timezero", 
                    return_lines = FALSE)

# Convert linestrings to .kml linestring
bathy_layer <- map_layers$bathymetry
bathy_layer$color <- sample(x = 1:11, 
                  size = nrow(bathy_layer ), 
                  replace = TRUE)

sf_to_kml_linestring(x = bathy_layer,
                     name_col = "METERS",
                     description_col = "LAST_UPDAT",
                     color_col = "color",
                     file = "test_lines.kml")


# Convert sf POLYGON to .kml polygons
strata <- map_layers$survey.strata
strata$color <- 5
strata$fill <- 0

dat <- sf::st_coordinates(strata$geometry[4])

as.data.frame(dat)

dat[,3]


sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  kml_file = "test_polygon.kml")


test <- sf::st_read("test_survey_marks.gpx")

plot(sf::st_read("test_polygon.kml"))

plot(sf::st_read("test_lines.kml"))
