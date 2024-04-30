#' Write sf data to a kml track file for TimeZero
#' 
#' @param x sf object that contains a POINT or LINESTRING geometry field
#' @param file Output file with a .kml extension
#' @param name_col Name of the column containing the linestring name.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param depth_col Name of the depth column that contains a numeric vector of depth values
#' @param ... Ignored
#' @export

sf_to_kml_track <- function(x, file, name_col, time_col = NULL, depth_col = NULL, ...) {

  .check_cols_exist(x = x, var_cols = c(name_col))
  
  if(any(sf::st_geometry_type(x) != "POINT")) {
    
    x <- sf::st_cast(x, to = "MULTIPOINT") |>
      sf::st_cast(to = "POINT")
  }
  
  .check_valid_geometry(x = x, valid = "POINT")
  
  .check_output_path(file = file, ext = ".kml")
  
  .check_extra_args(...)
  
  if(is.null(time_col)) {
    time_col <- "time"
    x[[time_col]] <- Sys.time()
  }
  
  x[[time_col]] <- paste0(format(x[[time_col]], "%Y-%m-%dT%H:%M:%S"), "Z")
  
  if(is.null(depth_col)) {
    depth_col <- "depth"
    depth_col <- 0
  }
  
  x[[depth_col]] <- round(x[[depth_col]], 1)
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  
  # Function to make track locations
  make_lines <- function(x, time_col, name_col, depth_col) {
    
    coords <- sf::st_coordinates(x[['geometry']])
    
    out <- paste0("        <gx:coord>", 
                  coords[, 1], " ", coords[, 2], " ", x[[depth_col]], 
                  "<gx:coord>")
    
    return(out)
  }
  
  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
             "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
             "  <Document>",
             "    <Placemark>",
             paste0("      <name>", x[[name_col]][1], "</name>"),
             "      <gx:Track>",
             paste0("        <when>", x[[time_col]], "</when>"),
             apply(X = x, 
                   MARGIN = 1, 
                   FUN = make_lines, 
                   depth_col = depth_col,
                   time_col = time_col, 
                   name_col = name_col),
             "      </gx:Track>",
             "    </Placemark>",
             "  </Document>",
             "</kml>"
  )
  
  message("sf_to_kml_linestring: Writing ", length(lines), " lines to ", file)
  
  con <- file(file)
  
  writeLines(text = unlist(lines), 
             con = con)
  
  close(con)

}