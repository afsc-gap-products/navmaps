#' Convert coordinates from WGS 84 Pseudo-Mercator (EPSG:3857) to WGS84 (EPSG:4326)
#'
#' This function takes a matrix of coordinates in EPSG:3857 (WGS 84 Pseudo-Mercator; used by TimeZero) and converts them to EPSG:4326 (WGS84). The input matrix is expected to have two columns, where the first column represents longitude and the second column represents latitude.
#'
#' @param x A matrix with two columns, where the first column is longitude and the second column is latitude, in EPSG:3857.
#' @return A matrix with two columns, where the first column is the converted longitude and the second column is the converted latitude, in EPSG:4326.
#'
#' @examples
#' # Example usage:
#' coordinates_3857 <- matrix(c(20037508.34, 0, 0, 20037508.34), ncol = 2, byrow = TRUE)
#' transformed_coordinates <- transform_3857_to_4326(coordinates_3857)
#'
#' @export

transform_3857_to_4326 <- function(x) {
  
  x_const <- 20037508.34
  
  # Convert longitude
  X <- (x[, 1]*180)/x_const
  
  # Convert latitude
  lat_4326 <- x[, 2] / (x_const / 180)
  
  exp_const <- (pi / 180) * lat_4326
  
  lat_4326 <- atan(exp(exp_const))
  Y <- lat_4326/(pi/360) - 90

  return(cbind(X, Y))
  
}



#' Convert coordinates from WGS84 (EPSG:4326) to WGS 84 Pseudo-Mercator (EPSG:3857)
#'
#' This function takes a matrix of coordinates in WGS84 (EPSG:4326) and converts them to EPSG:3857 (WGS 84 Pseudo-Mercator). The input matrix is expected to have two columns, where the first column islongitude and the second column is latitude.
#'
#' @param x A matrix with two columns, where the first column is longitude and the second column is latitude, in EPSG:4326.
#'
#' @return A matrix with two columns, where the first column is the converted longitude and the second column is the converted latitude, in EPSG:4326.
#'
#' @examples
#' # Example usage:
#' coordinates_4326 <- matrix(c(0, 0, 180, 90), ncol = 2, byrow = TRUE)
#' transformed_coordinates <- transform_4326_to_3857(coordinates_4326)
#'
#' @export
#' 
transform_4326_to_3857 <- function(x) {
  
  x_const <- 20037508.34
  
  X <- x[, 1] * x_const / 180
  
  lat_3857 <- (x[, 2] + 90) * (pi/360)
  
  Y <- (log(tan(lat_3857)) / (pi/180)) * x_const /180
  
  
  return(cbind(X, Y))
  
}
