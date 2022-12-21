#' Write sf data to a csv or mdb file formatted for globe
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .csv or .mdb extension.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing Globe color (decimal or rgb(?))
#' @param shape_col Name of the column containing Globe shape as an integer.
#' @param extra_cols Names of extra columns to include in output files, in the order the columns should be appended.
#' @export

sf_to_globe_points <- function(x, file, color_col, shape_col, time_col, extra_cols) {
  
  .check_cols_exist(x = x, var_cols = c(time_col, color_col, extra_cols))
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = c(".csv", ".mdb"))
  
  file_type <- tolower(strsplit(basename(file), split = "\\.")[[1]][-1])
  
  if(file_type %in% c("accdb", "mdb")) {
    .check_driver()
  }
  
  # Rename columns and convert lat/lon to radians to match Globe input format
  x <- cbind(x, as.data.frame(sf::st_coordinates(x))) |>
    dplyr::rename(Longitude = X, 
                  Latitude = Y) |>
    dplyr::mutate(Longitude = navmaps::dd_to_radians(Longitude),
                  Latitude = navmaps::dd_to_radians(Latitude)) |>
    dplyr::rename(Color = color_col,
                  Symbol = shape_col) |>
    as.data.frame()
  
  x$DateTime <- as.character(format(x[[time_col]], "%m/%d/%Y %I %r"))
  
  x <- dplyr::bind_rows(x,
                        dplyr::bind_rows(
                          data.frame(x[1,]), 
                          data.frame(ID = unique(x$ID)))[-1, ])
  
  out <- x |>
    dplyr::select(dplyr::all_of(c("Latitude", "Longitude", "Symbol", "Color", "DateTime", extra_cols)))
   
  if(file_type == "csv") {
    write.csv(out, file = file, row.names = FALSE, na = "")
  }
  
  if(file_type == ".mdb") {
    #FUTURE
  }
}


here::here()
