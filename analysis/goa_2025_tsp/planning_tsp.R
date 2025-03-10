# Traveling salesman solution to survey time

library(navmaps)

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.25 # Minimum processing time in hours
max_daily_stn = 6 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*9 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
set_on_arrival = TRUE # Set on arrival at the station or set based on the minimum processing time?
vessel = 148

# Ocean Explorer

# 2025 design 520 stations
vessel_dist_520 <- 
  sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

tsp_out_520 <- planning_solve_station_tsp(x = vessel_dist_520)

days_out_520 <- 
  planning_calc_survey_days(
    station_nodes = tsp_out_520$distance_nodes,
    max_daily_hr = max_daily_hr, 
    processing_time_hr = processing_time_hr,
    max_daily_stn = max_daily_stn, 
    transit_speed_kmh = transit_speed_kmh,
    set_retrieve_hr = set_retrieve_hr,
    set_on_arrival = set_on_arrival
  )


# 2025 400 stations
vessel_dist_400 <- 
  sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
  sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

tsp_out_400 <- planning_solve_station_tsp(x = vessel_dist_400)

days_out_400 <- 
  planning_calc_survey_days(
  station_nodes = tsp_out_400$distance_nodes,
  max_daily_hr = max_daily_hr, 
  processing_time_hr = processing_time_hr,
  max_daily_stn = max_daily_stn, 
  transit_speed_kmh = transit_speed_kmh,
  set_retrieve_hr = set_retrieve_hr,
  set_on_arrival = set_on_arrival
)

days_out_400$station_nodes$allocation <- 400
days_out_520$station_nodes$allocation <- 520

days_out_400$total_days
days_out_520$total_days

ggplot() +
  geom_histogram(data = dplyr::bind_rows(days_out_400$station_nodes,
                                         days_out_520$station_nodes),
                 mapping = aes(x = distance)) +
  facet_wrap(~allocation)


