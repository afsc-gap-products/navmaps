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
