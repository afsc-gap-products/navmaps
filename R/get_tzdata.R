#' Get Track Data from Timezero Database
#'
#' This function retrieves timezone data from a Timezero database file.
#'
#' @param path_tzdb A character string specifying the path to the Timezero database file (.tzdb).
#' 
#' @return A data frame containing the columns Date, x, and y from the database.
#' 
#' @details The function checks if the specified file exists and if it has a .tzdb extension.
#' It then connects to the database, queries the data, and disconnects. The resulting data frame
#' should have at least one row; otherwise, an error is raised.
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

get_tzdata <- function(path_tzdb){

  #check that file exists
  stopifnot("get_tzdata: path_tzdb file does not exist"=file.exists(path_tzdb))

  #check that the file is .tzdb
  stopifnot("get_tzdata: path_tzdb: must be a timezero database (.tzdb)"=grepl(x= path_tzdb, pattern= ".tzdb", ignore.case = TRUE))
 
   #connect to the database
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path_tzdb)
  
  #Query the database
  output <- RSQLite::dbGetQuery(con, "SELECT date, x, y FROM data")
  
  #Disconnect from the database
  RSQLite::dbDisconnect(con)
  
  #Atleast one row in output
  stopifnot("No data found in database"=nrow(output) > 0)
  

    return(output)
}
