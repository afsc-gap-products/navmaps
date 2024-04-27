#' Convert timezero data to gps format
#'
#' This function retrieves timezone and position data from a Timezero database file.
#'
#' @param path_tzdb A specific path to the Timezero database file (.tzdb).
#' 
#' @return A data frame containing the columns "Date", "X", and "Y" from the Timezero database.
#' 
#' @details The function checks if the specified file exists.
#' It then connects to the database, selects "Date", "X", and "Y", queries the data, and 
#' disconnects.
#' 
#' 
#' @examples
#' \dontrun{
#' # Provide the path to a valid Timezero database file
#' path <- "path/to/timezero_database.tzdb"
#' data <- get_tzdata(path)
#' }
#'
#' @import RSQLite
#' 
#' @export

convert_tz_to_gps <- function(path_tzdb, vessel, cruise, haul, start = NULL, end = NULL){
  
  # path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb") 
  # vessel = 162 
  # cruise = 202301
  # haul = 184
  
  
  x <- get_tzdata(path_tzdb = path_tzdb, start = start, end = end)

  x2 <- convert_tz_coords(x=x) 
  
  x3 <- convert_tz_date(x2)
  
  #convert decimal degrees to dmm 
  
  x3$X <- round(dd_to_dmm(x3$X), digits = 4)
  
  x3$Y <- round(dd_to_dmm(x3$Y), digits = 4)
  
  x3$vessel <- vessel

  x3$cruise <- cruise

  x3$haul <- haul
  
  output <- x3[,c("vessel", "cruise", "haul", "Date", "Y", "X")]
  
  write.table(output, file = "test.gps", row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

  return(output)
  
}


