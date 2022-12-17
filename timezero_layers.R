# Making layers for TimeZero

# Setup
library(navmaps)
region <- "ai"
map_layers <- akgfmaps::get_base_layers(select.region = region)

# Strata as a .kml polygon file
strata <- map_layers$survey.strata
strata$color <- 5
strata$fill <- 0

sf_to_kml_polygon(x = strata,
                  name_col = "Stratum",
                  description_col = "SURVEY",
                  color_col = "color",
                  fill_col = "fill",
                  file = here::here("output", region, paste0(region, "_survey_strata.kml")), 
                  software_format = "timezero")


# Station marks as a .gpx waypoint file (EBS/NBS only) 
grid_centers <- sf::st_centroid(map_layers$survey.grid)
grid_centers$shape <- 3
grid_centers$color <- 1

sf_to_gpx_waypoints(x = grid_centers, 
                    file = here::here("output", region, paste0(region, "_marks.gpx")), 
                    name_col = "STATIONID",
                    description_col = "STATIONID",
                    color_col = "color", 
                    shape_col = "shape", 
                    software_format = "timezero")

# Station grid without trawlable/untrawlable as a .kml linestring (EBS/NBS only) 
survey_grid <- map_layers$survey.grid
survey_grid$color <- 5
survey_grid$fill <- 0

sf_to_kml_linestring(x = survey_grid,
                     file = here::here("output", region, paste0(region, "_station_grid.kml")),
                     name_col = "STATIONID",
                     description_col = "STATIONID",
                     color_col = "color", 
                     software_format = "timezero")


# Station grid with trawlable/untrawlable as .kml and .shp polygons (AI/GOA only)
make_trawlable(region = region)

allocation_csv_path <- here::here("data", "allocation", "AIallocation420.csv")

# Station allocation as .gpx and .shp marks/points (AI/GOA only)
make_station_allocation(
  allocation_df = read.csv(file = allocation_csv_path) |>
    tidyr::drop_na(Longitude, Latitude),
  lon_col = "Longitude",
  lat_col = "Latitude",
  region = region,
  survey_year = NULL,
  station_col = "stationid",
  stratum_col = "stratum",
  vessel_col = "vessel",
  extra_col = "Priority",
  file_format = "gpx",
  software_format = "timezero"
)
