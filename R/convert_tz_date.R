#' Convert TimeZero date to POSIXct
#'
#' Converts integer 'Date' from a data.frame exported from a TimeZero SQLite database to a POSIXct formatted date.
#'
#' @param x A data.frame with a 'Date' column from TimeZero.
#' @export

convert_tz_date <- function(x){

  #Check that names of columns include Date, X, Y
  stopifnot("convert_tz_date: missing Date, X, and/or Y" = all(names(x) %in% c("Date","X","Y")))
  
  x <- x[complete.cases(x),]
  
  #Set Origin time January 1 2000 at 00:00:00 UTC
  x$Date <- as.POSIXct("2000-01-01", tz = "UTC") + x$Date
  
  x$Date <- format(x$Date, format = "%m/%d/%y %H:%M:%S")
  
  return(x)
  
}
