# Traveling salesman solution to survey time

# install.packages(c("sf", "TSP", "here", "dplyr"))

library(TSP)
library(here)
library(dplyr)
library(sf)

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.5 # Minimum processing time in hours
max_daily_stn = 6 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*8 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
set_on_arrival = FALSE # Set on arrival at the station or set based on the minimum processing time?
vessel = 148

# Calculations with station allocation ----

# Minimum distance using the traveling salesman problem package (TSP)

# Ocean Explorer

# 2023 example
vessel_dist <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
  sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
  dplyr::filter(vessel == "OEX") |>
  sf::st_transform(crs = "EPSG:32606")
  
# 2025 design
# vessel_dist <- sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
# sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
# dplyr::filter(VESSEL == vessel) # OEX

vessel_tsp <- 
  navmaps::planning_solve_station_tsp(
    x = vessel_dist
  )

vessel_days <- 
  planning_calc_survey_days(
    station_nodes = vessel_tsp$distance_nodes,
    max_daily_hr = max_daily_hr, 
    processing_time_hr = processing_time_hr,
    max_daily_stn = max_daily_stn, 
    transit_speed_kmh = transit_speed_kmh,
    set_retrieve_hr = set_retrieve_hr,
    set_on_arrival = TRUE
  )

vessel_days$station_nodes |>
  dplyr::group_by(day) |>
  dplyr::summarise(n_hauls = n()) |>
  dplyr::group_by(n_hauls) |>
  dplyr::summarise(n = n())

vessel_days$total_days
