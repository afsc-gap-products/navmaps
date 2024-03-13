#' Transform Timezero coordinates into compatible coordinates.
#'
#' This function converts the "X" and "Y" columns from the .tzdb database, converts to meters, transforms the coordinates from a coordinate reference system to a geographical coordinate system and replaces transformed "X" and "Y" back into data frame
#'
#' @param x A data.frame containing "Date", "X", and "Y"
#' 
#' @return A data frame with transformed "X" and "Y" coordinates
#' 
#' @import sf
#' @export


convert_tz_coords <- function(x){
 
  #Check that names of columns include Date, X, Y
  stopifnot("convert_tz_coords: missing Date, X, and/or Y"=all(names(x) %in% c("Date","X","Y")))
  
  #Remove incomplete cases
  
  #complete.cases()
  x <- x[complete.cases(x),]
  
  #Convert X and Y from cm to m (divide x and y by 100)
  x$X <- x$X/100
  x$Y <- x$Y/100
  
  #Convert to WGS84
  xy_mat <- as.matrix(x[, c("X", "Y")])
  xy_mat <- transform_3857_to_4326(xy_mat)
  
  #replaced columns with converted X, Y
  x$X <- xy_mat[,1]
  
  x$Y <- xy_mat[,2]
  
  
  #all X coordinates are between 0 and 180 and Y coordinates are between 0 and 90
  stopifnot("convert_tz_coords: X coordinates are not between 0 and 180 degrees" =all(abs(x$X) >= 0 & abs(x$X) <= 180))
  
  stopifnot("convert_tz_coords: Y coordinates are not between 0 and 90 degrees" =all(abs(x$Y) >= 0 & abs(x$Y) <= 90))
  
  
  return(x)
  
}
