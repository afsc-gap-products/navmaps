#' Write sf data to a csv or mdb file formatted for globe
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .csv or .mdb extension.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing Globe color (decimal or rgb(?))
#' @param shape_col Name of the column containing Globe shape as an integer.
#' @param extra_cols Names of extra columns to include in output files, in the order the columns should be appended.
#' @param ... Ignored
#' @export

sf_to_globe_points <- function(x, file, color_col, shape_col, time_col = NULL, name_col = NULL, description_col = NULL, extra_cols = NULL, ...) {
  
  .check_cols_exist(x = x, var_cols = c(time_col, color_col, shape_col, extra_cols))
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = c(".csv", ".mdb", ".accdb"))
  
  .check_extra_args(...)
  
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
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- Sys.time()
  }
  
  x$DateTime <- x[[time_col]]
  
  names(x)[which(names(x) %in% extra_cols)] <- toupper(names(x)[which(names(x) %in% extra_cols)])
  
  if(is.null(description_col)) {
    description_col <- "Comment"
  } else {
    x$Comment <- x[[description_col]] 
  }
  
  if(is.null(name_col)) {
    name_col <- "Name"
  } else {
    x$Name <- x[[name_col]] 
  }
  
  if("DEPTH" %in% names(x)) {
    x$Depth <- x$DEPTH
  } else {
    x$Depth <- as.numeric(NA)
  }
  
  if("TEMPERATURE" %in% names(x)) {
    x$Temperature <- x$TEMPERATURE
  } else {
    x$Temperature <- as.numeric(NA)
  }
  
  if("TIDE" %in% names(x)) {
    x$Tide <- x$TIDE
  } else {
    x$Tide <- as.numeric(NA)
  }
  
  if("LASTMODIFIED" %in% names(x)) {
    x$LastModified <- x$LASTMODIFIED
  } else {
    x$LastModified <- Sys.time()
  }
  
  # x$LastModified <- as.character(format(x[['LastModified']], "%m/%d/%Y %r"))
  
  if("FLAGS" %in% names(x)) {
    x$Flags <- x$FLAGS
  } else {
    x$Flags <- as.numeric(NA)
  }
  
  if("CATCH" %in% names(x)) {
    x$Flags <- x$CATCH
  } else {
    x$Catch <- as.numeric(NA)
  }

  # Check for required columns
  required_columns <- c("Latitude", "Longitude", "Symbol", "Color", "DateTime", "Name", "Comment", "Depth", "Temperature", "Tide", "Catch", "Flags", "LastModified")
  
  if(!(all(required_columns %in% names(x)))) {
    
    stop("sf_to_globe_points: Missing required column(s) ", paste(required_columns[which(!(required_columns %in% names(x)))], collapse = ", " ))
    
  }  
  
  out <- x |>
    dplyr::select(dplyr::all_of(c("Latitude", "Longitude", "Symbol", "Color", "DateTime", "Name", "Comment", "Depth", "Temperature", "Tide", "Catch", "Flags", "LastModified")))
  
   
  if(file_type == "csv") {
    write.csv(out, file = file, row.names = FALSE, na = "")
  }
  
  if(file_type %in%  c("mdb", "accdb")) {
    write_to_access(x = out,
                    dsn = file,
                    tablename = "marks",
                    append = TRUE,
                    drop_existing = FALSE)
  }
}
