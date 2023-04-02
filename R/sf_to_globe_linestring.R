#' Write sf data to a csv or mdb file formatted for globe
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .csv or .mdb extension.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing Globe color (decimal or rgb(?))
#' @param extra_cols Names of extra columns to include in output files, in the order the columns should be appended.
#' @param ... Ignored
#' @export

sf_to_globe_linestring <- function(x, file, color_col, time_col = NULL, extra_cols = NULL, ...) {
  
  .check_cols_exist(x = x, var_cols = c(time_col, color_col, extra_cols))
  
  .check_valid_geometry(x = x, valid = c("LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"))
  
  .check_output_path(file = file, ext = c(".csv", ".mdb", ".accdb"))
  
  .check_extra_args(...)
  
  # Retrieve file extension as a character vector
  file_type <- tolower(strsplit(basename(file), split = "\\.")[[1]][-1])
  
  if(file_type %in% c("accdb", "mdb")) {
    .check_driver()
  }
  
  # Convert complex polygon geometries
  if(any(sf::st_geometry_type(x) %in% c("MULTIPOLYGON"))) {
    x <- sf::st_cast(x, to = "POLYGON")
  }
  
  if(any(sf::st_geometry_type(x) %in% c("MULTILINESTRING"))) {
    x <- sf::st_cast(x, to = "LINESTRING")
  }
  
  x$ID <- 1:nrow(x)  
  
  x <- sf::st_cast(x, to = "POINT")
  
  # Rename columns and convert lat/lon to radians to match Globe input format
  x <- cbind(x, as.data.frame(sf::st_coordinates(x))) |>
    dplyr::rename(Longitude = X, 
                  Latitude = Y) |>
    dplyr::mutate(Longitude = navmaps::dd_to_radians(Longitude),
                  Latitude = navmaps::dd_to_radians(Latitude)) |>
    dplyr::rename(Color = color_col) |>
    dplyr::mutate(Width = -1) |>
    as.data.frame()
  
  x <- dplyr::inner_join(x,
                    data.frame(Color = globe_pal(n = Inf, type = "integer"),
                               replacement = globe_pal(n = Inf, type = "decimal")),
                    by = "Color")
  
  x$Color <- x$replacement
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- Sys.time()
  }
  
  x$DateTime <- x[[time_col]]
  
  # x$DateTime <- as.character(format(x[[time_col]], "%m/%d/%Y %r"))
  
  extra_cols <- toupper(extra_cols)
  
  if("HAULJOIN" %in% extra_cols) {
    x$hauljoin <- x$HAULJOIN
  } else {
    x$hauljoin <- as.numeric(NA)
  }
  
  x <- dplyr::bind_rows(x,
                        dplyr::bind_rows(
                          data.frame(x[1,]), 
                          data.frame(ID = unique(x$ID)))[-1, ]) |>
    dplyr::arrange(ID)
  
  x$index <- 1:nrow(x)
  
  # Check for required columns
  required_columns <- c("Latitude", "Longitude", "DateTime", "Color", "Width", "index", "hauljoin")
  
  if(!(all(required_columns %in% names(x)))) {
    
    stop("sf_to_globe_linestring: Missing required column(s) ", paste(required_columns[which(!(required_columns %in% names(x)))], collapse = ", " ))
  }  
  
  
  out <- x |>
    dplyr::select(dplyr::all_of(required_columns))
  
  if(file_type == "csv") {
    write.csv(out, file = file, row.names = FALSE, na = "")
  }
  
  if(file_type %in%  c("mdb", "accdb")) {
    write_to_access(x = out,
                    dsn = file,
                    tablename = "lines",
                    append = TRUE,
                    drop_existing = FALSE)
  }
}