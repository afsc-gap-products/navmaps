library(navmaps)
library(lubridate)

vessel = c(148, 176)
set_crs = "EPSG:3338"

# Maximum travel distance in a day in 2023
channel <- gapindex::get_connected()

goa_hauls_2023 <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
    "SELECT *
    FROM racebase.haul
    WHERE vessel IN (148, 176)
    AND cruise IN 202301
    AND performance >=0 ") |>
  dplyr::mutate(DOY = lubridate:::yday(START_TIME)) |>
  sf::st_as_sf(coords = c("START_LONGITUDE", "START_LATITUDE"), crs = "WGS84")

goa_hauls_lines <-
  goa_hauls_2023 |>
  dplyr::group_by(VESSEL, CRUISE, DOY) |>
  dplyr::summarize(do_union = FALSE,
                   n = n()) |> 
  sf::st_cast("LINESTRING") 

goa_hauls_lines$total_distance_km <-
  as.numeric(sf::st_length(goa_hauls_lines))/1e3

ggplot(data = goa_hauls_lines,
       mapping = aes(x = n,
                     y = total_distance_km)) +
  geom_point() +
  geom_smooth()



cumulative_dist_results <- vector(mode = "list", length = length(vessel))

for(jj in 1:length(vessel)) {
  
  station_520 <- 
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj])
  
  station_400 <-
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj])
  
  station_2023 <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
    sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
    dplyr::filter(vessel == ifelse(vessel[jj] == 148, "OEX", "AKP")) |>
    dplyr::select(-vessel) |>
    sf::st_transform(crs = set_crs) 
  
  cumulative_dist_520 <- 
    planning_build_clusters(
      nodes = station_520, 
      max_stations = 6
    ) |>
    dplyr::mutate(
      year = 2025,
      allocation = 520,
      vessel = vessel[jj]
    )
  
  cumulative_dist_400 <- 
    planning_build_clusters(
      nodes = station_400, 
      max_stations = 6
    ) |>
    dplyr::mutate(
      year = 2025,
      allocation = 400,
      vessel = vessel[jj]
    )
  
  cumulative_dist_520_2023 <- 
    planning_build_clusters(
      nodes = station_2023, 
      max_stations = 6
    ) |>
    dplyr::mutate(
      year = 2023,
      allocation = 520,
      vessel = vessel[jj]
    )
  
  cumulative_dist_results[[jj]] <- 
    dplyr::bind_rows(
      cumulative_dist_520,
                   cumulative_dist_400,
                   cumulative_dist_520_2023
      )
  
}

cumulative_dist_results <- 
  do.call(rbind, cumulative_dist_results)


cumulative_dist_summary <-
  cumulative_dist_results |>
  sf::st_drop_geometry() |>
  dplyr::group_by(allocation, year, station_rank, vessel) |>
  dplyr::summarise(mean_cumulative_distance_km = mean(cumulative_distance_km),
                   median_cumulative_distance_km = median(cumulative_distance_km))


# Maximum distance traveled in a day for a given number of hauls conducted
max_distance_by_n_2023 <-
goa_hauls_lines |>
  dplyr::ungroup() |>
  dplyr::rename(vessel = VESSEL) |>
  dplyr::group_by(vessel, n) |>
  dplyr::summarise(max_distance_km = max(total_distance_km),
                   median_distance_km = median(total_distance_km),
                   q90_cumulative_distance_km = quantile(total_distance_km, 0.9))

  ggplot() +
    geom_line(data = cumulative_dist_summary, 
              mapping = 
                aes(x = station_rank, 
                    y = mean_cumulative_distance_km, 
                    color = factor(allocation),
                    linetype = factor(year),
                    shape = factor(year))) +
    geom_point(data = cumulative_dist_summary, 
               mapping = 
                 aes(x = station_rank, 
                     y = mean_cumulative_distance_km, 
                     color = factor(allocation),
                     linetype = factor(year),
                     shape = factor(year))) +
    geom_path(data = max_distance_by_n_2023,
               mapping = aes(x = n, y = median_distance_km, color = "Median distance 2023")) +
    scale_x_continuous(name = "Station rank") +
    scale_y_continuous(name = "Mean cumulative distance (km)") +
    scale_color_colorblind(name = "Stations (#)") +
    scale_shape(name = "Year") +
    scale_linetype(name = "Year") +
    facet_wrap(~vessel) +
    theme_bw()
