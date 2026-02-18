library(navmaps)
library(gapindex)
library(ggthemes)
library(ggrepel)

# Setup parameters
vessel = c(148, 176)
set_crs = "EPSG:32606" # UTM Zone 6


# Calculate distances
station_tsp_dist <- vector(mode = "list", length = length(vessel))

for(jj in 1:length(vessel)) {
  
  days_out_2019 <-
    readxl::read_xlsx(
      here::here("assets", "data", "allocation", "GOA2019_ 2 boat_550_RNDM_stations.xlsx")
    ) |>
    dplyr::mutate(VESSEL = ifelse(vessel == "SEA", 143, 148)) |>
    dplyr::filter(VESSEL == ifelse(vessel[jj] == 176, 143, VESSEL)) |>
    dplyr::select(-vessel) |>
    sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
    dplyr::mutate(VESSEL =  ifelse(vessel[jj] == 176, 143, vessel[jj]),
                  YEAR = 2019,
                  allocation = 550) |>
    sf::st_transform(crs = set_crs) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
  days_out_2021 <-
    read.csv(
      file = here::here("assets", "data", "allocation", "GOA 2021 stations for Mark.csv")
    ) |>
    dplyr::mutate(VESSEL = ifelse(vessel == "OEX", 148, 176)) |>
    dplyr::select(-vessel) |>
    dplyr::filter(VESSEL == vessel[jj]) |>
    sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
    dplyr::mutate(VESSEL = ifelse(VESSEL == 148, 148, vessel[jj]),
                  YEAR = 2021,
                  allocation = 540) |>
    sf::st_transform(crs = set_crs) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
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
  
  # days_out_400 <-
  #   sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
  #   sf::st_transform(crs = set_crs) |>
  #   dplyr::filter(VESSEL == vessel[jj]) |>
  #   dplyr::mutate(YEAR = 2025,
  #                 allocation = 400) |>
  #   planning_solve_station_tsp(hamilton = TRUE)
  
  days_out_450 <-
    sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_450_aea.gpkg")) |>
    sf::st_transform(crs = set_crs) |>
    dplyr::filter(VESSEL == vessel[jj]) |>
    dplyr::mutate(YEAR = 2025,
                  allocation = 450) |>
    planning_solve_station_tsp(hamilton = TRUE)
  
  station_tsp_dist[[jj]] <-
    dplyr::bind_rows(
      days_out_2019$distance_nodes,
      days_out_2021$distance_nodes,
      days_out_2023$distance_nodes,
      days_out_520$distance_nodes,
      # days_out_400$distance_nodes,
      days_out_450$distance_nodes
    )
  
}

station_tsp_dist <- (do.call(rbind, station_tsp_dist))

station_tsp_dist <- 
  dplyr::select(
    station_tsp_dist,
    YEAR, 
    VESSEL, 
    node,
    allocation, 
    distance
    ) |>
  dplyr::mutate(
    id = paste0(YEAR, " ", allocation, " TSP")
    )

ggplot() +
  geom_histogram(
    data = station_tsp_dist,
    mapping = aes(x = distance/1.852, fill = factor(VESSEL)),
    bins = 20
  ) +
  facet_wrap(~paste0(YEAR, " (", allocation, ")")) +
  scale_x_continuous(name = "Allocation inter-station transit (nmi)") +
  scale_y_continuous(name = "Frequency") +
  scale_fill_tableau(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_histogram(
    data = station_tsp_dist,
    mapping = aes(x = distance/1.852/10, fill = factor(VESSEL)),
    bins = 20
  ) +
  facet_wrap(~paste0(YEAR, " (", allocation, ")")) +
  scale_x_continuous(name = "Transit at 10 knots (hr)") +
  scale_y_continuous(name = "Frequency") +
  scale_fill_tableau(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_density(
    data = station_tsp_dist,
    mapping = aes(x = distance/1.852, color = paste0(YEAR, " (", allocation, ")"), group = interaction(VESSEL, allocation, YEAR)),
    fill = NA
  ) +
  scale_x_continuous(name = "Allocation inter-station transit (nmi)") +
  scale_y_continuous(name = "Density") +
  scale_color_colorblind(name = "Year (stn)") + 
  theme_bw()

ggplot() +
  stat_ecdf(
    data = station_tsp_dist,
    mapping = aes(x = distance/1.852, color = paste0(YEAR, " (", allocation, ")"), group = interaction(VESSEL, allocation, YEAR))
  ) +
  scale_x_continuous(name = "Allocation inter-station transit (nmi)") +
  scale_y_continuous(name = "Cumulative proportion") +
  scale_color_colorblind(name = "Year") + 
  theme_bw()

ggplot() +
  geom_density(
    data = station_tsp_dist,
    mapping = aes(x = distance/1.852, color = paste0(YEAR, " (", allocation, ")"), group = interaction(VESSEL, allocation, YEAR)),
    fill = NA
  ) +
  scale_x_continuous(name = "Transit at 10 knots (hr)") +
  scale_y_continuous(name = "Density") +
  scale_color_colorblind(name = "Year") + 
  theme_bw()


total_transit <- 
  station_tsp_dist |>
  sf::st_drop_geometry() |>
  dplyr::group_by(VESSEL, YEAR, allocation) |>
  dplyr::summarise(total_dist = sum(distance)) |>
  dplyr::inner_join(station_tsp_dist |>
                      sf::st_drop_geometry() |>
                      dplyr::mutate(
                        transit_time = distance/1.852/10,
                        transit_gt_1hr = transit_time > 1,
                        transit_gt_1_5hr = transit_time > 1.5,
                        transit_gt_2hr = transit_time > 2,
                        transit_gt_3hr = transit_time > 3
                      ) |>
                      dplyr::group_by(VESSEL, YEAR, allocation) |>
                      dplyr::summarise(
                        p_transit_gt_1hr = sum(transit_gt_1hr)/n()*100,
                        p_transit_gt_1_5hr = sum(transit_gt_1_5hr)/n()*100,
                        p_transit_gt_2hr = sum(transit_gt_2hr)/n()*100,
                        p_transit_gt_3hr = sum(transit_gt_3hr)/n()*100,
                        n_transit = n())
  ) |>
  dplyr::arrange(YEAR)

ggplot() +
  geom_point(
    data = total_transit,
    mapping = aes(x = YEAR, y = total_dist, color = factor(VESSEL))
  ) +
  geom_text(
    data = total_transit,
    mapping = aes(x = YEAR, y = total_dist, color = factor(VESSEL), label = allocation),
    hjust = -0.2
  ) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Total distance (km)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_point(
    data = total_transit,
    mapping = aes(x = YEAR, y = total_dist/n_transit, color = factor(VESSEL))
  ) +
  geom_text(
    data = total_transit,
    mapping = aes(x = YEAR, y = total_dist/n_transit, color = factor(VESSEL), label = allocation),
    hjust = -0.2
  ) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Transit distance per station (km)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_point(data = total_transit,
             mapping = aes(x = YEAR, y = p_transit_gt_1hr, color = factor(VESSEL))) +
  geom_text_repel(data = total_transit,
                  mapping = aes(x = YEAR, y = p_transit_gt_1hr, color = factor(VESSEL), label = allocation),
                  hjust = -0.2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Transit > 1hr @ 10 knots (%)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_point(data = total_transit,
             mapping = aes(x = YEAR, y = p_transit_gt_1_5hr, color = factor(VESSEL))) +
  geom_text_repel(data = total_transit,
                  mapping = aes(x = YEAR, y = p_transit_gt_1_5hr, color = factor(VESSEL), label = allocation),
                  hjust = -0.2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Transit > 1.5hr @ 10 knots (%)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_point(data = total_transit,
            mapping = aes(x = YEAR, y = p_transit_gt_2hr, color = factor(VESSEL))) +
  geom_text_repel(data = total_transit,
             mapping = aes(x = YEAR, y = p_transit_gt_2hr, color = factor(VESSEL), label = allocation),
            hjust = -0.2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Transit > 2hr @ 10 knots (%)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()

ggplot() +
  geom_point(data = total_transit,
             mapping = aes(x = YEAR, y = p_transit_gt_3hr, color = factor(VESSEL))) +
  geom_text_repel(data = total_transit,
                  mapping = aes(x = YEAR, y = p_transit_gt_3hr, color = factor(VESSEL), label = allocation),
                  hjust = -0.2) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Transit > 3hr @ 10 knots (%)") +
  scale_color_colorblind(name = "Vessel") +
  theme_bw()


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

# Compare distances between stations
ggplot() +
  stat_ecdf(
    data = dplyr::filter(station_tsp_dist, YEAR %in% c(2023, 2025)),
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
  facet_wrap(~VESSEL) +
  theme_bw() +
  theme(legend.title = element_blank())

# Compare survey travel distance to TSP distance solution

survey_travel_dist <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT * FROM RACEBASE.HAUL WHERE VESSEL IN (148, 176, 143) AND CRUISE >= 201901 AND REGION = 'GOA' AND PERFORMANCE >= 0")  |>
  sf::st_as_sf(crs = "WGS84", coords = c("START_LONGITUDE", "START_LATITUDE")) |>
  sf::st_transform(crs = "EPSG:32606") |>
  dplyr::mutate(YEAR = floor(CRUISE/100)) |>
  dplyr::arrange(VESSEL, YEAR, HAUL) |>
  dplyr::group_by(VESSEL, YEAR) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::mutate(distance_km = as.numeric(sf::st_length(geometry)/1e3)) |>
  sf::st_drop_geometry()
