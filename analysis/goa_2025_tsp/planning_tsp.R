# Traveling salesman solution to survey time

library(TSP)
library(here)
library(dplyr)
library(sf)

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.5 # Minimum processing time in hours
max_daily_stn = 6 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*7.5 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
set_on_arrival = TRUE # Set on arrival at the station or set based on the minimum processing time?
vessel = 148

# Ocean Explorer

# 2025 design
vessel_dist <- sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

tsp_out <- planning_solve_station_tsp(x = vessel_dist)

days_out <- 
  planning_calc_survey_days(
  station_nodes = tsp_out$distance_nodes,
  max_daily_hr = max_daily_hr, 
  processing_time_hr = processing_time_hr,
  max_daily_stn = max_daily_stn, 
  transit_speed_kmh = transit_speed_kmh,
  set_retrieve_hr = set_retrieve_hr,
  set_on_arrival = set_on_arrival
)

tsp_out$distance_nodes[45:55,]

days_out$station_nodes[45:55,]

days_out$station_nodes$day
