# Demonstration of the tow search tool

# Install packages as needed

install.packages("remotes", "tmap")

remotes::install_github(repo = "afsc-gap-products/akgfmaps")

remotes::install_github(repo = "afsc-gap-products/navmaps")

library(akgfmaps)
library(navmaps)
library(tmap)
library(lubridate)

# Setup data that would be internal

channel <- navmaps::get_connected(schema = "afsc")

# Get haul performance data for the AI
perf_dat <- RODBC::sqlQuery(channel = channel,
                            query = "select h.haul, h.vessel, h.cruise, h.performance, h.start_time, h.stratum, h.stationid from
racebase.haul h, race_data.cruises c, race_data.surveys s
where s.survey_definition_id = 52
and s.survey_id = c.survey_id
and c.vessel_id = h.vessel
and c.cruise = h.cruise
and h.cruise > 198400") |>
  dplyr::mutate(GOOD_HAUL = PERFORMANCE >= 0) |> # Assign good/bad
  dplyr::rename(ID = STATIONID) # Change ID to match ID fields in the AI grid shapefile

perf_counts <- perf_dat |>
  dplyr::group_by(STRATUM, ID) |>
  dplyr::summarise(LAST_TOW_YEAR = factor(max(lubridate::year(START_TIME))),
                   N_GOOD = sum(GOOD_HAUL),
                   N_BAD = sum(!GOOD_HAUL)
  ) |>
  dplyr::mutate(PCT_GOOD = round(100*N_GOOD/(N_GOOD+N_BAD)))

# Get trawlable/untrawlable designations
trawlable <- RODBC::sqlQuery(channel = channel,
                             query = "select aigrid_id, trawlable, stratum, stationid from ai.aigrid_gis") |>
  dplyr::mutate(TRAWLABLE = ifelse(is.na(TRAWLABLE), 'U', TRAWLABLE)) |>
  dplyr::rename(ID = STATIONID) # Change ID to match ID fields in the AI grid shapefile


# Find alternative grid cells within the stratum
alt_stations <- tow_helper(region = "ai",
                           stationid = "35-48",
                           stratum = 214)

# Join everything
alt_stations <- alt_stations |>
  dplyr::inner_join(trawlable) |>
  dplyr::left_join(perf_counts) |>
  dplyr::mutate(N_GOOD = ifelse(is.na(N_GOOD), 0, N_GOOD),
                N_BAD = ifelse(is.na(N_BAD), 0, N_BAD)) |>
  dplyr::mutate(GOOD_BAD = paste0(N_GOOD, "/", N_GOOD+N_BAD, " (", PCT_GOOD, "%)")) |>
  dplyr::select(ID, STRATUM, TRAWLABLE, LAST_TOW_YEAR, GOOD_BAD, PCT_GOOD, dist_nmi) 

# This should work in offline mode
tmap_mode("view")

tmap::tm_shape(alt_stations) +
  tmap::tm_symbols() +
  tmap::tm_symbols(fill = "dist_nmi") +
  tmap::tm_polygons(fill = "TRAWLABLE")

tmap_mode("view")
tmap::tm_shape(alt_stations) +
  tmap::tm_polygons() +
  tmap::tm_polygons(fill = "PCT_GOOD")

tmap_mode("view")
tmap::tm_shape(alt_stations) +
  tmap::tm_symbols() +
  tmap::tm_symbols(fill = "PCT_GOOD") +
  tmap::tm_polygons(fill = "TRAWLABLE")

