library(navmaps)
library(ggthemes)

# Setup parameters
vessel = c(148, 176)
set_crs = "EPSG:3338"

station_plan_distances <- data.frame()

for(jj in 1:length(vessel)) {
  
  # 2025 design 520 stations
  station_520 <- 
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj])
  
  # 2025 design 400 stations
  station_400 <-
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj])
  
  # 2023 design 520 stations
  station_2023 <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
    dplyr::rename(VESSEL = vessel) |>
    sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
    dplyr::filter(VESSEL == ifelse(vessel[jj] == 148, "OEX", "AKP")) |>
    sf::st_transform(crs = set_crs) 
  
  station_plan_distances <- 
    rbind(
      station_plan_distances,
      data.frame(distance_m = 
                   c(planning_station_proximity(x = station_520, ith_station = 5)$mean_ith_station_m,
                     planning_station_proximity(x = station_520, ith_station = 4)$mean_ith_station_m,
                     planning_station_proximity(x = station_520, ith_station = 3)$mean_ith_station_m,
                     planning_station_proximity(x = station_520, ith_station = 2)$mean_ith_station_m,
                     planning_station_proximity(x = station_520)$mean_nearest_m),
                 sum_nearest_m = planning_station_proximity(x = station_520)$sum_nearest_m,
                 station_proximity = 5:1,
                 station_plan = 520,
                 year = 2025,
                 vessel = jj
      ), 
      data.frame(distance_m = 
                   c(planning_station_proximity(x = station_400, ith_station = 5)$mean_ith_station_m,
                     planning_station_proximity(x = station_400, ith_station = 4)$mean_ith_station_m,
                     planning_station_proximity(x = station_400, ith_station = 3)$mean_ith_station_m,
                     planning_station_proximity(x = station_400, ith_station = 2)$mean_ith_station_m,
                     planning_station_proximity(x = station_400)$mean_nearest_m),
                 sum_nearest_m = planning_station_proximity(x = station_400)$sum_nearest_m,
                 station_proximity = 5:1,
                 station_plan = 400,
                 year = 2025,
                 vessel = jj
      ),
      data.frame(distance_m = 
                   c(planning_station_proximity(x = station_2023, ith_station = 5)$mean_ith_station_m,
                     planning_station_proximity(x = station_2023, ith_station = 4)$mean_ith_station_m,
                     planning_station_proximity(x = station_2023, ith_station = 3)$mean_ith_station_m,
                     planning_station_proximity(x = station_2023, ith_station = 2)$mean_ith_station_m,
                     planning_station_proximity(x = station_2023)$mean_nearest_m),
                 sum_nearest_m = planning_station_proximity(x = station_2023)$sum_nearest_m,
                 station_proximity = 5:1,
                 station_plan = 520,
                 year = 2023,
                 vessel = jj
      )
    )
  
  
}


ggplot(data = station_plan_distances,
       mapping = 
         aes(
           x = station_proximity, 
           y = distance_m/1e3, 
           color = factor(station_plan),
           shape = factor(vessel),
           linetype = factor(vessel)
         )) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "ith nearest station") +
  scale_y_continuous(name = "Distance (km)") +
  scale_color_colorblind(name = "Stations (#)") +
  scale_shape(name = "Vessel") +
  scale_linetype(name = "Vessel") +
  facet_wrap(~year) +
  theme_bw()


ggplot(data = dplyr::filter(station_plan_distances, station_plan == 520),
       mapping = 
         aes(
           x = station_proximity, 
           y = distance_m/1e3, 
           color = factor(year),
           shape = factor(vessel),
           linetype = factor(vessel)
         )) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "ith nearest station") +
  scale_y_continuous(name = "Distance (km)") +
  scale_color_colorblind(name = "Stations (#)") +
  scale_shape(name = "Vessel") +
  scale_linetype(name = "Vessel") +
  theme_bw()


station_plan_diff_pct <- station_plan_distances |>
  dplyr::select(-sum_nearest_m) |>
  tidyr::pivot_wider(names_from = "station_plan", values_from = distance_m) |>
  dplyr::mutate(diff_distance_m = `400` - `520`,
                diff_distance_pct = 100*(`400` - `520`)/`520`)

ggplot(data = station_plan_diff_pct,
       mapping = 
         aes(x = station_proximity, 
             y = diff_distance_m, 
             color = factor(vessel),
             shape = factor(vessel),
             linetype = factor(vessel)
         )
) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "ith nearest station") +
  # scale_y_continuous(name = expression(bar(D[400])-bar(D[520]))*' (km)') +
  scale_shape(name = "Vessel") +
  scale_linetype(name = "Vessel") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()


ggplot(data = station_plan_diff_pct,
       mapping = 
         aes(x = station_proximity, 
             y = diff_distance_pct, 
             color = factor(vessel),
             shape = factor(vessel),
             linetype = factor(vessel)
         )
) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "ith nearest station") +
  scale_y_continuous(name = expression(100%*%over(bar(D[400])-bar(D[520]),bar(D[520]))*' (%)')) +
  scale_shape(name = "Vessel") +
  scale_linetype(name = "Vessel") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

# Shortest path for each node
nodes <- planning_station_proximity(x = station_520)$nearest_stations
nodes$index <- 1:nrow(nodes)
start_index <- 1
max_visits <- 6

find_shortest_path <- function(nodes, start_index, max_visits = 6) {
  visited <- start_index  # Start from the given node
  current_index <- start_index
  
  while(length(visited) < max_visits) {
    # Get nearest unvisited node
    next_node <- nodes$nearest_index[nodes$index == current_index]
    
    # If next_node is already visited, find another closest node
    if (next_node %in% visited) {
      available_nodes <- nodes[!nodes$index %in% visited, ]
      next_node <- available_nodes$index[which.min(available_nodes$nearest_m)]
    }
    
    # Add to the visited list
    visited <- c(visited, next_node)
    current_index <- next_node
  }
  
  return(visited)
}
