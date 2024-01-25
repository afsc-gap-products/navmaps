#' Create a test  function
#'
#' A function that adds numeric vector a and b 
#'
#' @param a a numeric vector
#' @param b a number vector
#' @return a number
#' @export
#' @import lubridate


convert_tz_date <- function(x, output_timezone= "America/Anchorage"){
  
  ##############
  x= get_tzdata(path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb"))
  ##############
  #Check that names of columns include Date, X, Y
  
  #Remove incomplete cases
  #complete.cases()
  
  #Set Origin time
  as.numeric(as.POSIXct("2000-01-01 12:00", tz = "UTC"))
  origin <- as.POSIXct("2000-01-01 12:00", tz = "UTC")
  today <- as.POSIXct("2000-01-01 12:00", tz = "UTC") +25465431
  as.numeric(today)-as.numeric(origin)
  
  #Add number to origin time
  
  return()
  
  
}