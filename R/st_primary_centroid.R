#' Compute Centroids for Polygons and Largest Polygon in Multipolygons
#'
#' This function calculates centroids for an `sf` object containing `POLYGON` and/or `MULTIPOLYGON` geometries. For `POLYGON`s it returns the centroid of the polygon. For `MULTIPOLYGON`s, it computes the centroid of the largest constituent polygon.
#'
#' @param x An `sf` object with `POLYGON` or `MULTIPOLYGON` geometries.
#'
#' @return An `sf` object of centroid points with original attributes preserved.
#' @import sf
#' @export

st_primary_centroid <- function(x) {
  
  stopifnot(inherits(x, "sf"))
  
  # Loop through each feature
  centroid_list <- lapply(seq_len(nrow(x)), function(i) {
    geom <- sf::st_geometry(x[i, ])[[1]]
    geom_type <- sf::st_geometry_type(geom, by_geometry = FALSE)
    
    if (geom_type == "POLYGON") {
      sf::st_centroid(sf::st_sfc(geom, crs = sf::st_crs(x)))
    } else if (geom_type == "MULTIPOLYGON") {
      # Convert MULTIPOLYGON to POLYGON components
      polygons <- sf::st_cast(sf::st_sfc(geom, crs = sf::st_crs(x)), "POLYGON")
      areas <- sf::st_area(polygons)
      largest <- polygons[which.max(areas)]
      sf::st_centroid(largest)
    } else {
      stop("Unsupported geometry type: ", geom_type)
    }
  })
  
  centroid_geom <- do.call(what = rbind, args = centroid_list)
  
  # Combine centroids into sf object
  centroid_sf <- sf::st_sf(x, geometry = centroid_geom, crs = sf::st_crs(x))
  return(centroid_sf)
}
