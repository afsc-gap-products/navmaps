#' Read data from a TimeZero database and convert to a .gps file
#'
#' This function retrieves time and position data from a TimeZero database and converts it to a .gps
#' file format.
#' 
#' @param path_tzdb A specific path to the TimeZero database file (.tzdb).
#' @param output_file Output file path as a character vector.
#' @param vessel RACE vessel number
#' @param cruise RACE cruise number
#' @param haul RACE haul number
#' @param start Optional start time as character vector POSIXct object for filtering data from the TimeZero database.
#' @param end Optional end time as character vector or POSIXct object for filtering ddata from the TimeZero database.
#' @return A data frame containing the columns "Date", "X", and "Y" from the Timezero database.
#' @examples
#' \dontrun{
#' # Path to a valid TimeZero database file
#'path <- system.file("extdata", "OwnShipRecorder.tzdb", package = "navmaps")
#' 
#' convert_tz_to_gps(path_tzdb = path,
#'                   output_file = NULL,
#'                   vessel = 176,
#'                   cruise = 202401,
#'                   haul = 22,
#'                   start = "01/25/2024 14:30:00",
#'                   end = "01/25/2024 14:33:15")
#' }
#' @import RSQLite
#' @export

convert_tz_to_gps <- function(path_tzdb, 
                              output_file = NULL, 
                              vessel, 
                              cruise, 
                              haul, 
                              start = NULL, 
                              end = NULL) {
  
  if(is.null(output_file)) {
    output_file <- paste0(getwd(), 
                          "/", 
                          "HAUL", gsub(paste0(".{", nchar(haul), "}$"), haul, "0000"), ".gps")
  }
  
  x <- get_tz_data(path_tzdb = path_tzdb, start = start, end = end)

  x2 <- convert_tz_coords(x=x) 
  
  x3 <- convert_tz_date(x2)
  
  x3$X <- round(dd_to_dmm(x3$X), digits = 4)
  
  x3$Y <- round(dd_to_dmm(x3$Y), digits = 4)
  
  x3$vessel <- vessel

  x3$cruise <- cruise

  x3$haul <- haul
  
  output <- x3[,c("vessel", "cruise", "haul", "Date", "Y", "X")]
  
  message("convert_tz_to_gps: Writing output to ", output_file)
  
  write.table(output, file = output_file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")

  return(output)
  
}



#' Get vessel track data from a from TimeZero Database
#'
#' This function retrieves timezone and position data from a Timezero database file.
#'
#' @param path_tzdb A specific path to the Timezero database file (.tzdb).
#' @param start Optional start time as character vector POSIXct object for filtering data from the TimeZero database.
#' @param end Optional end time as character vector or POSIXct object for filtering ddata from the TimeZero database.
#' @return A data frame containing the columns "Date", "X", and "Y" from the Timezero database.
#' 
#' @details The function checks if the specified file exists.
#' It then connects to the database, selects "Date", "X", and "Y", queries the data, and 
#' disconnects.
#' @examples
#' \dontrun{
#' # Provide the path to a valid Timezero database file
#' path <- "path/to/timezero_database.tzdb"
#' data <- get_tz_data(path)
#' }
#'
#' @import RSQLite
#' 
#' @export

get_tz_data <- function(path_tzdb, start = NULL, end = NULL){
  
  query <- "SELECT date, x, y FROM data"
  
  if(!is.null(start) & !is.null(end)){ 
    
    origin <- as.numeric(as.POSIXct("2000-01-01", tz = "UTC"))
    
    start <- as.POSIXct(start, 
                        tz = "America/Anchorage", 
                        tryFormats = c("%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%OS", "%m/%d/%Y %H:%M")) 
    
    end <- as.POSIXct(end, 
                      tz = "America/Anchorage", 
                      tryFormats = c("%m/%d/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%OS", "%m/%d/%Y %H:%M"))
    
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



#' Convert dmm format to decimal degrees
#'
#' Convert degree decimal minute coordinates to decimal degrees (e.g. 16530.5 to 165.5083)
#' @param x Numeric degree decimal minute coordinats
#' @export

dmm_to_dd <- function(x) {
  abs_x <- abs(x)
  sign(x) * (abs(abs_x) %/% 100 + (abs_x %% 100)/60)
}



#' Convert decimal degree to dmm
#'
#' Convert degree decimal coordinates to degree decimal minutes (e.g. 165.5083 to 16530.5)
#' @param x numeric decimal degree coordinates
#' @export

dd_to_dmm <- function(x){
  
  output <- (floor(abs(x)) * 100 + (x %% 1) * 60) * sign(x)
  
  return(output)
  
}



#' Convert coordinates from WGS84 (EPSG:4326) to WGS 84 Pseudo-Mercator (EPSG:3857)
#'
#' This function takes a matrix of coordinates in WGS84 (EPSG:4326) and converts them to EPSG:3857 (WGS 84 Pseudo-Mercator). The input matrix is expected to have two columns, where the first column is longitude and the second column is latitude.
#'
#' @param x A matrix with two columns, where the first column is longitude and the second column is latitude, in EPSG:4326.
#'
#' @return A matrix with two columns, where the first column is the converted longitude and the second column is the converted latitude, in EPSG:4326.
#'
#' @examples
#' # Example usage:
#' coordinates_4326 <- matrix(c(-172, -160, 40, 55), ncol = 2)
#' transformed_coordinates <- transform_4326_to_3857(coordinates_4326)
#'
#' @export

transform_4326_to_3857 <- function(x) {
  
  x_const <- 20037508.34
  
  X <- x[, 1] * x_const / 180
  
  lat_3857 <- (x[, 2] + 90) * (pi/360)
  
  Y <- (log(tan(lat_3857)) / (pi/180)) * x_const /180
  
  
  return(cbind(X, Y))
  
}



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





