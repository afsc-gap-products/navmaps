#' Calculate station proximity metrics for survey planning
#'
#' This function calculates the proximity of points and summary statistics related to the nearest 
#' distances between stations. It also allows for computation of the n-th nearest station and 
#' distances computes various summary statistics related to the nearest distances between stations.
#'
#' @param x An `sf` object containing only POINT features (e.g., planning stations).
#' @param ith_station Should a distance be calculated for the ith nearest station to each station (e.g., 4th nearest station to each station.). If numeric, output includes distance to the ith nearest station. If NULL or 1, only returns the distance to the nearest station. (Default: `ith_station = NULL`)
#' 
#' @return A list containing:
#' \item{nearest_stations}{A data frame with the original attributes of `x`, the index of the nearest station, 
#' the distance to the nearest station, and the original geometry.}
#' \item{n}{The number of stations.}
#' \item{min_nearest_m}{The minimum distance to the nearest station.}
#' \item{max_nearest_m}{The maximum distance to the nearest station.}
#' \item{sum_nearest_m}{The sum of all nearest station distances.}
#' \item{mean_nearest_m}{The mean distance to the nearest station.}
#' \item{median_nearest_m}{The median distance to the nearest station.}
#'
#' @import sf
#' @export


planning_station_proximity <- function(x, ith_station = NULL) {
  
  stopifnot("planning_station_proximity: x must be an sf or sfc class." = is(x, "sf") | is(x, "sfc"))
  stopifnot("planning_station_proximity: All geometries in 'x' must 
            be POINT types. Non-point types detected." = 
              all(
                sf::st_geometry_type(x) == "POINT"
              )
  )
  
  # Minimum distance greater than zero
  min_not_zero <- function(zz, min_rank = 1) {
    
    z_dist <- zz[zz > 0]
    
    if(min_rank > 1) {
      out <- z_dist[rank(z_dist) == min_rank]
    } else {
      
      out <- min(z_dist)
    }
    
    out
    
  }
  
  # Index of the station for the minimum distance greater than zero
  which_min_no_zero <- function(zz) {
    zz[zz <= 0] <- NA
    which.min(zz)
  }
  
  nearest_index <- 
    apply(
      X = sf::st_distance(x, x), 
      MARGIN = 1, 
      FUN = which_min_no_zero
    )
  
  nearest_m <- 
    apply(
      X = sf::st_distance(x, x), 
      MARGIN = 1, 
      FUN = min_not_zero
    )
  
  out <- list(
    nearest_stations =
      cbind(
        sf::st_drop_geometry(x),
        nearest_index,
        nearest_m,
        sf::st_geometry(x)),
    n = nrow(x),
    min_nearest_m = min(nearest_m),
    max_nearest_m = max(nearest_m),
    sum_nearest_m = sum(nearest_m),
    mean_nearest_m = mean(nearest_m),
    median_nearest_m = median(nearest_m)
  )
  
  if(!is.null(ith_station)) {
    
    ith_station_m <- 
      apply(
        X = sf::st_distance(x, x), 
        MARGIN = 1, 
        FUN = min_not_zero,
        min_rank = ith_station
      )
    
    out[['nearest_stations']]['ith_station_m'] <- ith_station_m
    out[['mean_ith_station_m']] <- mean(ith_station_m)
    out[['median_ith_station_m']] <- median(ith_station_m)
    
  }
  
  return(out)
  
}