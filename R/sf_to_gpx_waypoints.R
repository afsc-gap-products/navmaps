#' Write sf point data to a gpx file
#' 
#' Write sf POINT geometries to a gpx waypoint file.
#' 
#' @param x data.frame containing latitude, longitude, name, color, and shape columns.
#' @param file Output file with a .gpx extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing integer colors.
#' @param shape_col Name of the column containing integer shapes.
#' @param ... Ignored
#' @export

sf_to_gpx_waypoints <- function(x, file, name_col, description_col, time_col = NULL, color_col, shape_col, ...) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col, shape_col))
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = ".gpx")
  
  .check_extra_args(...)
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[[time_col]] <- paste0(format(x[[time_col]], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  
  lines <- c("<?xml version=\"1.0\"?>",
             "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\">")
  
  # Functions to make waypoints (vectorized)
  make_lines <- paste <- function(x, time_col, name_col, description_col, shape_col, color_col) {
    coords <- sf::st_coordinates(x[['geometry']])
    
    out <- paste0("  <wpt lat=\"", coords[, 2], "\" lon=\"", coords[, 1], "\">\n",
                  "    <time>", x[time_col], "</time>\n",
                  "    <name>", x[name_col], "</name>\n",
                  "    <cmt>", x[description_col], "</cmt>\n",
                  "    <extensions>\n",
                  "      <MxTimeZeroSymbol>", x[shape_col], "</MxTimeZeroSymbol>\n",
                  "      <T0Color>", x[color_col], "</T0Color>\n",
                  "    </extensions>\n",
                  "  </wpt>\n"
                  )
    return(out)
  }
  
  lines <- c(lines,
             apply(X = x, 
                   MARGIN = 1, 
                   FUN = make_lines, 
                   time_col = time_col, 
                   name_col = name_col, 
                   description_col = description_col, 
                   shape_col = shape_col, 
                   color_col = color_col), 
             "</gpx>"
  )
  
  message("sf_to_gpx: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
}
