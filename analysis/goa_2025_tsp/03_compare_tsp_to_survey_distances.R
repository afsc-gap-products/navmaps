
# 2023 

vessel_dist_2023 <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
  sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
  dplyr::filter(vessel == "OEX") |>
  sf::st_transform(crs = "EPSG:32606")

tsp_out_2023 <- planning_solve_station_tsp(x = vessel_dist_2023)

days_out_2023 <- 
  planning_calc_survey_days(
    station_nodes = tsp_out_2023$distance_nodes,
    max_daily_hr = max_daily_hr, 
    processing_time_hr = processing_time_hr,
    max_daily_stn = max_daily_stn, 
    transit_speed_kmh = transit_speed_kmh,
    set_retrieve_hr = set_retrieve_hr,
    set_on_arrival = set_on_arrival
  )

ggplot() +
  stat_ecdf(
    data = dplyr::bind_rows(days_out_400$station_nodes,
                            days_out_520$station_nodes) |>
      dplyr::select(VESSEL, distance, allocation),
    mapping = aes(x = distance, color = paste0(allocation, " station TSP")),
    linewidth = rel(1.2)
  ) +
  stat_ecdf(
    data = goa_2023_hauls,
    mapping = aes(x = distance_km, color = "2023 Survey"),
    linewidth = rel(1.2)
  ) +
  stat_ecdf(
    data = days_out_2023$station_nodes, 
    mapping = aes(x = distance, color = "2023 520 station TSP"),
    linewidth = rel(1.2)
  ) +
  scale_color_manual(
    values = c(
      "400 station TSP" = "cyan", 
      "520 station TSP" = "blue", 
      "2023 Survey" = "black", 
      "2023 520 station TSP" = "grey50"
    )
  ) +
  scale_x_continuous(name = "Distance (km)") +
  scale_y_continuous(name = "Cumulative proportion") +
  theme(legend.title = element_blank())
