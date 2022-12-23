# Make layers for TimeZero

# 1. Setup
library(navmaps)
region <- "ai"
software <- "timezero"

# Load shapefiles using the akgfmaps package
map_layers <- akgfmaps::get_base_layers(select.region = region)
channel <- get_connected(schema = "AFSC")

# 2. Get data
get_gps_data(region = region, channel = channel)

# 3. Build layers
# Strata as a .kml polygon file
strata <- map_layers$survey.strata
strata$color <- navmaps_pal(values = "yellow", software_format = software, file_type = "kml")
strata$fill <- 0

# Defaults to creating a kml POLYGON
sf_to_nav_file(
  x = strata,
  file = here::here("output", region, "navigation", paste0(region, "_survey_strata.kml")), 
  name_col = "Stratum",
  description_col = "Stratum",
  color_col = "color",
  fill_col = "fill",
  software_format = software
)

# Force to create a LINESTRING
# sf_to_nav_file(
#   x = strata,
#   geometry = "LINESTRING", # <-- sets the sf geometry type
#   file = here::here("output", region, "navigation", paste0(region, "_survey_strata.kml")), 
#   name_col = "Stratum",
#   description_col = "Stratum",
#   color_col = "color",
#   fill_col = "fill",
#   software_format = software
# )


# Station marks in a .gpx waypoint file (EBS/NBS only) 
grid_centers <- sf::st_centroid(map_layers$survey.grid) # Points at the center of each grid cell
grid_centers$shape <- navmaps_sym_pal(values = "circle1", software_format = software, file_type = "gpx")
grid_centers$color <- navmaps_pal(values = "yellow", software_format = software, file_type = "gpx")

sf_to_nav_file(x = grid_centers,
               file = here::here("output", region, "navigation", paste0(region, "_marks.gpx")),
               name_col = "STATIONID",
               description_col = "STATIONID",
               color_col = "color",
               shape_col = "shape",
               software_format = software)

# Station grid (no trawlable/untrawlable) as a .kml linestring (EBS/NBS only) 
survey_grid <- map_layers$survey.grid
survey_grid$color <- navmaps_pal(values = "tan", software_format = software, file_type = "kml")
survey_grid$fill <- 0

sf_to_nav_file(
  x = survey_grid,
  geometry = "LINESTRING",
  file = here::here("output", region, "navigation", paste0(region, "_station_grid.kml")),
  name_col = "STATIONID",
  description_col = "STATIONID",
  color_col = "color", 
  software_format = software
)

# Station grid with trawlable/untrawlable cells as .kml and .shp polygons (AI/GOA only)
make_trawlable(
  region = region, 
  channel = channel)

# Station allocation as .gpx and .shp marks/points (AI/GOA only)
read.csv(here::here("data", "allocation", "AIallocation420.csv")) |>
  tidyr::drop_na(Longitude, Latitude) |>
  make_station_allocation(
    lon_col = "Longitude",
    lat_col = "Latitude",
    region = region,
    station_col = "stationid",
    stratum_col = "stratum",
    vessel_col = "vessel",
    extra_col = "Priority",
    software_format = software
  )


# Historical tow start and midpoint as .gpx and .shp marks/points and towpaths as .kml linestring
make_towpaths(region = region, 
              overwrite_midpoint = FALSE, 
              software_format = software)


# SSL buffer zones as .kml polygon



# No transit zones as .kml polygon



# Crab pot storage as .kml linestring



# Historical catch data as .shp points



# 4. Special projects


