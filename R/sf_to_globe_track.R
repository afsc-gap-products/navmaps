#' Write sf data to a csv or mdb file formatted for globe
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .csv or .mdb extension.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing Globe color (decimal or rgb(?))
#' @param depth_col Name of the depth column that contains a numeric vector of depth values
#' @param index_col Name of the index column that contains a numeric vector of track indices
#' @param ... Ignored
#' @export

sf_to_globe_track <- function(x, file, time_col = NULL, color_col = NULL, depth_col = NULL, index_col = NULL, ...) {
  
  .check_cols_exist(x = x, var_cols = time_col)
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = c(".csv", ".mdb", ".accdb"))
  
  file_type <- tolower(strsplit(basename(file), split = "\\.")[[1]][-1])
  
  if(file_type %in% c("accdb", "mdb")) {
    .check_driver()
  }
  
  x <- sf::st_transform(x, crs = "WGS84")
  
  # Rename columns and convert lat/lon to radians to match Globe input format
  x <- cbind(x, as.data.frame(sf::st_coordinates(x))) |>
    dplyr::rename(Longitude = X, 
                  Latitude = Y) |>
    dplyr::mutate(Longitude = navmaps::dd_to_radians(Longitude),
                  Latitude = navmaps::dd_to_radians(Latitude)) |>
    as.data.frame()
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- Sys.time()
  }
  
  x$DateTime <- x[[time_col]]
  
  if(is.null(depth_col)) {
    depth_col <- "depth"
    x$depth <- as.numeric(NA)
  }
  
  x$Depth <- x[[depth_col]]
  
  if(is.null(index_col)) {
    index_col <- "index"
    x$index <- 1:nrow(x)
  }
  
  x$index <- x[[index_col]]
  
  if(is.null(color_col)) {
    color_col <- "color"
    x$color <- 255
  }
  
  x$Color <- x[[color_col]]
  
  # Check for required columns
  required_columns <- c("Latitude", "Longitude", "Color", "DateTime", "Depth", "index")
  
  if(!(all(required_columns %in% names(x)))) {
    
    stop("sf_to_globe_points: Missing required column(s) ", 
         paste(required_columns[which(!(required_columns %in% names(x)))], 
               collapse = ", " ))
    
  }  
  
  out <- x |>
    dplyr::select(dplyr::all_of(c("Latitude", "Longitude", "DateTime", "Color", "Depth", "index")))
  
  if(file_type == "csv") {
    write.csv(out, file = file, row.names = FALSE, na = "")
  }
  
  if(file_type %in%  c("mdb", "accdb")) {
    write_to_access(x = out,
                    dsn = file,
                    tablename = "Track",
                    append = TRUE,
                    drop_existing = FALSE)
  }
}