#' Write sf data to a kml point file
#' 
#' Write sf POINT geometries to a kml point file
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing eight-digit hex color.
#' @param shape_col Name of the column containing integer shapes.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @param ... Ignored
#' @export

sf_to_kml_points <- function(x, file, name_col, description_col, shape_col = NULL, time_col = NULL, color_col, ...) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col))
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = ".kml")
  
  .check_extra_args(...)
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[[time_col]] <- paste0(format(x[[time_col]], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  
  # Function to make marks (vectorized)
  make_lines <- paste <- function(x, time_col, name_col, description_col, shape_col, color_col) {
    coords <- sf::st_coordinates(x[['geometry']])
    
    out <- paste0("    <Placemark>\n",
                  "    <name>", x[name_col], "</time>\n",
                  "    <description>", x[description_col], "</description>\n",
                  "      <TimeStamp><when>", x[time_col], "</when></TimeStamp>\n",
                  "      <Point>\n",
                  "        <coordinates>\n",
                  paste0(coords[, 1], ",", coords[, 2], "\n"),
                  "        </coordinates>\n",
                  "      </Point>\n",
                  "      <Style>\n",
                  "        <IconStyle>\n",
                  paste0("        <color>", x[color_col], "</color>\n"),
                  "        </IconStyle>\n",
                  "      </Style>\n",
                  "    </Placemark>\n"
    )
    
    return(out)
  }
  
  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
             "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
             "  <Document>",
             apply(X = x, 
                   MARGIN = 1, 
                   FUN = make_lines, 
                   time_col = time_col, 
                   name_col = name_col, 
                   description_col = description_col, 
                   shape_col = shape_col, 
                   color_col = color_col),
             "  </Document>",
             "</kml>"
  )
  
  message("sf_to_kml_linestring: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
}