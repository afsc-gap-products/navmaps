#' Create a test  function
#'
#' A function that adds numeric vector a and b 
#'
#' @param a a numeric vector
#' @param b a number vector
#' @return a number
#' @export
#' @import sf


convert_tz_coords <- function(x){
 
  #Check that names of columns include Date, X, Y
  stopifnot("convert_tz_coords: missing Date, X, and/or Y"=all(names(x) %in% c("Date","X","Y")))
  
  #Remove incomplete cases
  
  #complete.cases()
  x <- x[complete.cases(x),]
  
  #Convert X and Y from cm to m (divide x and y by 100)
  x$X <- x$X/100
  x$Y <- x$Y/100
  
  #Convert X to simple features object using sf::st_as_sf
  x_sf <- sf::st_as_sf(x=x, coords = c("X", "Y"), crs = "EPSG:3857")
  x_sf <- sf::st_transform(x_sf, crs= "WGS84")
  
  #Transform to WGS84
  
  xy_mat <- sf::st_coordinates(x_sf)
 
  #replaced columns with converted X, Y
  x$X <- xy_mat[,1]
  
  x$Y <- xy_mat[,2]
  
  
  #all X coordinates are between 0 and 180 and Y coordinates are between 0 and 90
  stopifnot("convert_tz_coords: X coordinates are not between 0 and 180 degrees" =all(abs(x$X) >= 0 & abs(x$X) <= 180))
  
  stopifnot("convert_tz_coords: Y coordinates are not between 0 and 90 degrees" =all(abs(x$Y) >= 0 & abs(x$Y) <= 90))
  
  
  return(x)
  
}