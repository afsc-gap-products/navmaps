#' Help find alternative stations to tow in a stratum
#' 
#' Finds stations in the same stratum and calculates the distance to the stations.
#'
#' @param stationid Station ID as a character vector (e.g. "38-57")
#' @param stratum Stratum as a numeric vector (e.g. 214)
#' @param grid_res_m Grid resolution as a numeric vector, used for distance calculations
#' @param grid_buffer_m Buffer around the edges of the grid, used for distance calculations
#' @import tmap dplyr sf terra akgfmaps

tow_helper <- function(stationid, stratum, region, grid_res_m = 250, grid_buffer_m = 40000) {

  map_layers <- akgfmaps::get_base_layers(select.region = region,
                                          set.crs = "WGS84")

  x <- map_layers$survey.grid |>
    dplyr::select(GRID_ID, STATION, STRATUM)

  land_mask <- map_layers$akland

  if(is.null(stratum)) {
    stratum = x$STRATUM[x$ID == stationid]
  }

  stratum_stns <- x[x$STRATUM == stratum, ]

  station_loc <- x[x$STATION == stationid, ]

  utm_zone <-  (stratum_stns |>
    sf::st_point_on_surface() |>
    sf::st_coordinates())[,1] |>
    navmaps:::longitude_to_utm_zone() |>
    table() |>
    sort() |>
    names() |>
    dplyr::last()

  utm_crs <- paste0("EPSG:326", utm_zone)

  stratum_cells <- stratum_stns |>
    sf::st_transform(crs = utm_crs)

  stratum_stns <- stratum_stns |>
    sf::st_point_on_surface() |>
    sf::st_transform(crs = utm_crs)
  
  station_loc <- station_loc |>
    sf::st_point_on_surface() |>
    sf::st_transform(crs = utm_crs)
  
  land_mask <- land_mask |>
    sf::st_transform(crs = utm_crs)

  station_bbox <- sf::st_bbox(stratum_stns)

  cost_rast <- terra::rast(resolution = grid_res_m,
              xmin = station_bbox[['xmin']] - grid_buffer_m,
              xmax = station_bbox[['xmax']] + grid_buffer_m,
              ymin = station_bbox[['ymin']] - grid_buffer_m,
              ymax = station_bbox[['ymax']] + grid_buffer_m,
              crs = utm_crs, vals = 1)

  cost_rast <- terra::mask(cost_rast, land_mask, inverse = TRUE)

  icell <- terra::cellFromXY(cost_rast, sf::st_coordinates(station_loc))

  cost_rast[icell] <- 0

  cost_rast <- terra::gridDist(cost_rast, target = 0)

  cost_rast <- terra::mask(cost_rast, stratum_cells, touches = TRUE)

  cost_poly <- cost_rast |>
    terra::as.polygons() |>
    sf::st_as_sf()

  alternative_stations <- sf::st_intersection(stratum_stns, cost_poly) |>
    dplyr::mutate(dist_nmi = units::as_units(layer/1000/1.852
, value = "nmi")) |>
    sf::st_drop_geometry() |>
    dplyr::select(STATION, GRID_ID, dist_nmi, STRATUM)

  alternative_stations <- dplyr::left_join(stratum_cells, alternative_stations)

  return(alternative_stations)

}



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
and h.cruise > 199000") |>
  dplyr::mutate(GOOD_HAUL = PERFORMANCE >= 0) |> # Assign good/bad
  dplyr::rename(STATION = STATIONID) # Change ID to match ID fields in the AI grid shapefile

perf_counts <- perf_dat |>
  dplyr::group_by(STRATUM, STATION) |>
  dplyr::summarise(LAST_TOW_YEAR = factor(max(lubridate::year(START_TIME))),
                   N_GOOD = sum(GOOD_HAUL),
                   N_BAD = sum(!GOOD_HAUL)
  ) |>
  dplyr::mutate(PCT_GOOD = round(100*N_GOOD/(N_GOOD+N_BAD)))

# Get trawlable/untrawlable designations
trawlable <- RODBC::sqlQuery(channel = channel,
                             query = "select aigrid_id, trawlable, stratum, stationid from ai.aigrid_gis") |>
  dplyr::mutate(TRAWLABLE = ifelse(is.na(TRAWLABLE), 'U', TRAWLABLE)) |>
  dplyr::rename(STATION = STATIONID) # Change ID to match ID fields in the AI grid shapefile


# Find alternative grid cells within the stratum
alt_stations <- 
  tow_helper(
    stationid = "258-36", 
    stratum = 622, 
    region = "ai",
    grid_res_m = 1000
  )

# Join everything
alt_stations <- alt_stations |>
  dplyr::inner_join(trawlable) |>
  dplyr::left_join(perf_counts) |>
  dplyr::mutate(N_GOOD = ifelse(is.na(N_GOOD), 0, N_GOOD),
                N_BAD = ifelse(is.na(N_BAD), 0, N_BAD)) |>
  dplyr::mutate(GOOD_BAD = paste0(N_GOOD, "/", N_GOOD+N_BAD, " (", PCT_GOOD, "%)")) |>
  dplyr::select(STATION, STRATUM, TRAWLABLE, LAST_TOW_YEAR, GOOD_BAD, PCT_GOOD, dist_nmi) 

tmap_mode("view")

tmap::tm_shape(dplyr::filter(alt_stations, TRAWLABLE == "Y")) +
  tmap::tm_polygons(fill = "dist_nmi")

