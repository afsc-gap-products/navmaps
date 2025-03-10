library(gapindex)
library(lubridate)
library(dplyr)

dir.create(here::here("analysis", "goa_2025_tsp"))

channel <- gapindex::get_connected()

dat <- RODBC::sqlQuery(
  channel = channel,
  query = "SELECT * FROM RACEBASE.HAUL WHERE VESSEL IN (148, 176) AND CRUISE = 202301 AND PERFORMANCE >= 0")

dat$DOY <- lubridate::yday(dat$START_TIME)

hauls_per_day <- 
  dat |>
  dplyr::group_by(DOY, VESSEL) |>
  dplyr::summarise(n = n())

successful_haul_freq <- 
  hauls_per_day |>
  dplyr::group_by(VESSEL, n) |>
  dplyr::summarise(successful_hauls = n()) |>
  dplyr::mutate(bin_total = successful_hauls * n)

write.csv(successful_haul_freq, here::here("analysis", "goa_2025_tsp", "successful_hauls_freq_2023.csv"), row.names = FALSE)

ggplot() +
  geom_histogram(hauls_per_day,
                 mapping = aes(x = n),
                 bins = 6) +
  scale_y_continuous(name = "# Days") +
  scale_x_continuous(name = "Successful hauls") +
  facet_wrap(~VESSEL)

gapindex_data <- gapindex::get_data(year_set = 2023, survey_set = "GOA")

gapindex_data$haul


dplyr::select(dat, VESSEL, DOY) |>
  unique() |>
  dplyr::group_by(VESSEL) |>
  dplyr::summarise(n = n())


start_end_time <- 
  dat |>
  dplyr::group_by(DOY, VESSEL) |>
  dplyr::summarise(min_time = min(START_TIME),
                   max_time = max(START_TIME)) |>
  dplyr::mutate(FIRST_LAST_START_TIME_DIFF_HR = as.numeric(difftime(max_time, min_time, units = "hours")))


ggplot() +
  geom_histogram(data = start_end_time,
                 mapping = aes(x = FIRST_LAST_START_TIME_DIFF_HR))

ggplot() +
  stat_ecdf(data = start_end_time,
                 mapping = aes(x = FIRST_LAST_START_TIME_DIFF_HR, color = factor(VESSEL))) +
  scale_x_continuous(name = "Daily Last-First START_TIME (hr)") +
  scale_y_continuous(name = "Cumulative proportion") +
  scale_color_discrete(name = "Vessel")

time_between_starts <- as.numeric(diff(dat$START_TIME)/60)
time_between_starts <- time_between_starts[time_between_starts < 12 & time_between_starts > 0]

quantile(time_between_starts, 0.05)


ggplot() +
  stat_ecdf(mapping = aes(time_between_starts)) +
  scale_x_continuous(name = "Time between starts (hr)") +
  scale_y_continuous(name = "Cumulative proportion")


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

ggplot() +
  geom_histogram(data = goa_2023_hauls,
                 mapping = aes(x = distance_km,
                               fill = factor(VESSEL))) +
  geom_vline(xintercept = median(goa_2023_hauls$distance_km), linetype = 2) +
  scale_fill_manual(name = "Vessel", values = c("red", "yellow")) +
  scale_x_continuous(name = "Transit distance (km)") +
  scale_y_continuous(name = "Frequency")
  
