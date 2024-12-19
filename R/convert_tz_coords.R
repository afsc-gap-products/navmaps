#' Transform TimeZero coordinates to WGS84
#'
#' This function converts the X and Y coordinates from a .tzdb database to WGS84 coordinates.
#'
#' @param x A data.frame containing "Date", "X", and "Y"
#' @return A data frame with transformed "X" and "Y" coordinates
#' @import sf
#' @export

convert_tz_coords <- function(x){
 
  #Check that names of columns include Date, X, Y
  stopifnot("convert_tz_coords: missing Date, X, and/or Y"=all(names(x) %in% c("Date","X","Y")))
  
  x <- x[complete.cases(x),]
  
  x$X <- x$X/100
  
  x$Y <- x$Y/100
  
  xy_mat <- as.matrix(x[, c("X", "Y")])
  
  xy_mat <- transform_3857_to_4326(xy_mat)
  
  x$X <- xy_mat[,1]
  
  x$Y <- xy_mat[,2]
  
  stopifnot("convert_tz_coords: X coordinates are not between 0 and 180 degrees" =all(abs(x$X) >= 0 & abs(x$X) <= 180))
  
  stopifnot("convert_tz_coords: Y coordinates are not between 0 and 90 degrees" =all(abs(x$Y) >= 0 & abs(x$Y) <= 90))
  
  return(x)
  
}
