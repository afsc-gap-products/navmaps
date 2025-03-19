library(navmaps)
library(gapindex)
library(ggthemes)

# Setup parameters
vessel = c(148, 176)
set_crs = "EPSG:32606" # UTM Zone 6


# Calculate distances
station_tsp_dist <- vector(mode = "list", length = length(vessel))

for(jj in 1:length(vessel)) {
  
  days_out_2023 <- 
    read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
    sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
    dplyr::filter(vessel == ifelse(vessel[jj] == 148, "OEX", "AKP")) |>
    dplyr::select(-vessel) |>
    dplyr::mutate(VESSEL =  vessel[jj],
                  YEAR = 2023,
                  allocation = 520) |>
    sf::st_transform(crs = set_crs) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
  days_out_520 <- 
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj]) |>
    dplyr::mutate(YEAR = 2025,
                  allocation = 520) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
  days_out_400 <- 
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj]) |>
    dplyr::mutate(YEAR = 2025,
                  allocation = 400) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
  station_tsp_dist[[jj]] <-
    dplyr::bind_rows(
      days_out_2023$distance_nodes,
      days_out_520$distance_nodes,
      days_out_400$distance_nodes
    )
  
}

station_tsp_dist <- (do.call(rbind, station_tsp_dist))
station_tsp_dist <- 
  dplyr::select(
    station_tsp_dist,
    YEAR, 
    VESSEL, 
    allocation, 
    distance
    ) |>
  dplyr::mutate(
    id = paste0(YEAR, " ", allocation, " TSP")
    )


# Actual distance between stations in 2023

channel <- gapindex::get_connected()

dat <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT * FROM RACEBASE.HAUL WHERE VESSEL IN (148, 176) AND CRUISE = 202301 AND PERFORMANCE >= 0")

# Distance between stations
oex_hauls_sf <- dat |>
  dplyr::filter(VESSEL == 148) |>
  sf::st_as_sf(crs = "WGS84", coords = c("START_LONGITUDE", "START_LATITUDE")) |>
  sf::st_transform(crs = "EPSG:32606")

oex_hauls_sf$distance_km <- c(0,
                              sf::st_distance(oex_hauls_sf[1:(nrow(oex_hauls_sf)-1), ],
                                              oex_hauls_sf[2:nrow(oex_hauls_sf), ],
                                              by_element = TRUE)/1000 |>
                                as.numeric()
)

akp_hauls_sf <- dat |>
  dplyr::filter(VESSEL == 176) |>
  sf::st_as_sf(crs = "WGS84", coords = c("START_LONGITUDE", "START_LATITUDE")) |>
  sf::st_transform(crs = "EPSG:32606")

akp_hauls_sf$distance_km <- c(0,
                              sf::st_distance(akp_hauls_sf[1:(nrow(akp_hauls_sf)-1), ],
                                              akp_hauls_sf[2:nrow(akp_hauls_sf), ],
                                              by_element = TRUE)/1000 |>
                                as.numeric()
)

goa_2023_hauls <- dplyr::bind_rows(oex_hauls_sf, akp_hauls_sf)

# Compare cumulative distances


ggplot() +
  stat_ecdf(
    data = station_tsp_dist,
    mapping = aes(x = distance, color = id),
    linewidth = rel(1.2)
  ) +
  stat_ecdf(
    data = goa_2023_hauls,
    mapping = aes(x = distance_km, color = "2023 Survey"),
    linewidth = rel(1.2)
  ) +
  scale_color_manual(
    values = c(
      "2025 400 TSP" = "cyan",
      "2025 520 TSP" = "blue",
      "2023 Survey" = "black",
      "2023 520 TSP" = "grey50"
    )
  ) +
  scale_x_continuous(name = "Distance (km)") +
  scale_y_continuous(name = "Cumulative proportion") +
  theme_bw() +
  theme(legend.title = element_blank())
