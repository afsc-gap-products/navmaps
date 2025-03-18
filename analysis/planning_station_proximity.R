library(navmaps)

# Setup parameters
vessel = 176

# Alaska Provider

# 2025 design 520 stations
vessel_dist <- 
  sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:3338") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

# 2025 design 400 stations
# vessel_dist <- 
#   sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_station_allocation_400.gpkg")) |>
#   sf::st_transform(crs = "EPSG:3338") |> # UTM Zone 6 CRS
#   dplyr::filter(VESSEL == vessel) # OEX


planning_station_proximity <- 
  function(x) {
    
    min_not_zero <- function(zz) {
      min(zz[zz > 0])
    }
    
    which_min_no_zero <- function(zz) {
      zz[zz <= 0] <- NA
      which.min(zz)
    }
    
    
    nearest_index <- apply(X = sf::st_distance(x, x), MARGIN = 1, FUN = which_min_no_zero)
    nearest_m <- apply(X = sf::st_distance(x, x), MARGIN = 1, FUN = min_not_zero)
    
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
    
    return(out)
    
  }

planning_station_proximity(x = vessel_dist)

