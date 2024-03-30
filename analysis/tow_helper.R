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
    dplyr::select(AIGRID_ID, ID, STRATUM)

  land_mask <- map_layers$akland

  if(is.null(stratum)) {
    stratum = x$STRATUM[x$ID == stationid]
  }

  stratum_stns <- x[x$STRATUM == stratum, ]

  station_loc <- x[x$ID == stationid, ]

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
    dplyr::mutate(dist_nmi = units::as_units(lyr.1/1000/1.852
, value = "nmi")) |>
    sf::st_drop_geometry() |>
    dplyr::select(ID, AIGRID_ID, dist_nmi, STRATUM)

  alternative_stations <- dplyr::left_join(stratum_cells, alternative_stations)

  return(alternative_stations)

}
