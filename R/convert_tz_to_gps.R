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
#' path <- "path/to/timezero_database.tzdb"
#' data <- get_tzdata(path)
#' convert_tz_to_gps <- function(path_tzdb = path,
#'                               output_file = NULL,
#'                               vessel = 176,
#'                               cruise = 202201,
#'                               haul = 22,
#'                               start = 11/10/2022 11:46:56,
#'                               end = 11/10/2022 11:59:12)
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


