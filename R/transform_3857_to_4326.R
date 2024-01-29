#' Convert coordinates from WGS 84 Pseudo-Mercator (EPSG:3857) to WGS84 (EPSG:4326)
#'
#' This function takes a matrix of coordinates in EPSG:3857 (WGS 84 Pseudo-Mercator; used by Time Zero) and converts them to EPSG:4326 (WGS84). The input matrix is expected to have two columns, where the first column represents longitude and the second column represents latitude.
#'
#' @param x A matrix with two columns, where the first column is longitude and the second column is latitude, in EPSG:3857.
#' @return A matrix with two columns, where the first column is the converted longitude and the second column is the converted latitude, in EPSG:4326.
#'
#' @examples
#' # Example usage:
#' coordinates_3857 <- matrix(c(-19146952, -17811119, 4865942, 7361866), ncol = 2)
#' transformed_coordinates <- transform_3857_to_4326(coordinates_3857)
#'
#' @export

transform_3857_to_4326 <- function(x) {
  
  x_const <- 20037508.34
  
  X <- (x[, 1]*180)/x_const
  
  lat_4326 <- x[, 2] / (x_const / 180)
  
  exp_const <- (pi / 180) * lat_4326
  
  Y <- atan(exp(exp_const))/(pi/360) - 90
  
  return(cbind(X, Y))
  
}