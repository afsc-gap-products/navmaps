#' Get Track Data from Timezero Database
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

get_tzdata <- function(path_tzdb, start = NULL, end = NULL){

  # start = "2024-01-25 00:00:00"
  # 
  # path_tzdb= here::here("assets", "data", "OwnShipRecorder.tzdb")
  # 
  # end = "2024-01-25 06:00:00"
  
  query <- "SELECT date, x, y FROM data"
  
  if(!is.null(start) & !is.null(end)){ 
    
    origin <- as.numeric(as.POSIXct("2000-01-01", tz = "UTC"))
    
    start <- as.POSIXct(start, tz = "America/Anchorage") + 3600*8
    
    end <- as.POSIXct(end, tz = "America/Anchorage") + 3600*8
    
    start <- as.numeric(start) - origin
    
    end <- as.numeric(end) - origin
    
    query <- paste0("SELECT date, x, y FROM data WHERE date >= ", start, " and date <= ", end)
  }
  
  #check that file exists
  stopifnot("get_tzdata: path_tzdb file does not exist" = file.exists(path_tzdb))

  #check that the file is .tzdb
  stopifnot("get_tzdata: path_tzdb: must be a timezero database (.tzdb)"= grepl(x = path_tzdb, pattern = ".tzdb", ignore.case = TRUE))
 
   #connect to the database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path_tzdb)
  
  #Query the database
  output <- RSQLite::dbGetQuery(con, query)
  
  #Disconnect from the database
  RSQLite::dbDisconnect(con)
  
  #Atleast one row in output
  stopifnot("No data found in database" = nrow(output) > 0)
  

    return(output)
}
