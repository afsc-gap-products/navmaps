#' Set global variables for file extensions and software type
#' 
#' @param x software name as a character vector. Options: globe, opencpn, timezero
#' @param globe_type File type for globe "mdb" (default) or "csv"
#' @export

set_software <- function(x, globe_type = "mdb") {
  
  .check_software(software_format = x)
  
  message("Assigning FILE_TYPE_POLYGON, FILE_TYPE_LINESTRING, FILE_TYPE_POINT, and SOFTWARE to the the global environment.")
  
  assign("SOFTWARE", value = x, envir = .GlobalEnv)
  
  if(x == "timezero") {
    assign("FILE_TYPE_POLYGON", value = "kml", envir = .GlobalEnv)
    assign("FILE_TYPE_LINESTRING", value = "kml", envir = .GlobalEnv)
    assign("FILE_TYPE_POINT", value = "gpx", envir = .GlobalEnv)
  }
  
  if(x == "opencpn") {
    assign("FILE_TYPE_POLYGON", value = "gpx", envir = .GlobalEnv)
    assign("FILE_TYPE_LINESTRING", value = "gpx", envir = .GlobalEnv)
    assign("FILE_TYPE_POINT", value = "gpx", envir = .GlobalEnv)
  }
  
  if(x == "globe") {
    assign("FILE_TYPE_POLYGON", value = "mdb", envir = .GlobalEnv)
    assign("FILE_TYPE_LINESTRING", value = "mdb", envir = .GlobalEnv)
    assign("FILE_TYPE_POINT", value = "mdb", envir = .GlobalEnv)
  }
  
  if(x == "globe" & globe_type == "csv") {
    assign("FILE_TYPE_POLYGON", value = "csv", envir = .GlobalEnv)
    assign("FILE_TYPE_LINESTRING", value = "csv", envir = .GlobalEnv)
    assign("FILE_TYPE_POINT", value = "csv", envir = .GlobalEnv)
  }
  
}