#' Write sf data to a kml linestring file
#' 
#' Write sf LINESTRING geometries to a kml linestring file
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing eight-digit hex color.
#' @param ... Ignored
#' @export

sf_to_kml_linestring <- function(x, file, name_col, description_col, time_col = NULL, color_col, ...) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col))
  
  .check_valid_geometry(x = x, valid = c("LINESTRING", "MULTIPOLYGON", "POLYGON"))
  
  .check_output_path(file = file, ext = ".kml")
  
  .check_extra_args(...)
  
  # Remove invalid geometries
  x <- remove_invalid_geometry(x = x)
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[[time_col]] <- paste0(format(x[[time_col]], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  
  make_lines <- function(x, time_col, name_col, description_col, color_col) {
    coords <- sf::st_coordinates(x[['geometry']])
    
    # Handle invalid cases with single points
    if(nrow(coords) < 2) {
      return(NULL)
    }
    
    n_segments <- unique(coords[, 3])
    
    out <- character()
    for(ii in 1:length(n_segments)) {
      sel_coords <- coords[coords[, 3] == ii, ]
      
      out <- paste0("    <Placemark>\n",
                    "      <name>", x[name_col], "</name>\n",
                    "      <description>", x[description_col], "</description>\n",
                    "      <time>", x[time_col], "</time>\n",
                    "      <LineString>\n",
                    "        <coordinates>\n        ",
                    paste(
                      paste(sel_coords[ , 1], sel_coords[ , 2], sep = ", "), 
                      collapse = "\n        "
                    ), "\n",
                    "        </coordinates>\n",
                    "      </LineString>\n",
                    "      <Style>\n",
                    "        <LineStyle>\n",
                    "        <color>", x[color_col], "</color>\n",
                    "        </LineStyle>\n",
                    "      </Style>\n",
                    "    </Placemark>"
      )
    }
    
    out <- paste(out, collapse = "\n")
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
                   color_col = color_col),
             "  </Document>",
            "</kml>")
  
  # Drop NULL
  lines <- unlist(lines[sapply(lines, is.character)])
  
  message("sf_to_kml_linestring: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
}
