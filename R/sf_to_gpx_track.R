#' Write sf to gpx track file
#' 
#' Write sf LINESTRING, POLYGON, MULTIPOLYGON, or MULTILINESTRING geometries to a gpx file
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param description_col Description column.
#' @param color_col Color column name. Optional. If not provided, color is set to black.
#' @param ... Ignored
#' @export

sf_to_gpx_track  <- function(x, file, name_col, time_col = NULL, color_col = NULL, description_col, ...) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col))
  
  .check_valid_geometry(x, valid = c("LINESTRING", "POLYGON", "MULTIPOLYGON", "MULTILINESTRING"))
  
  .check_output_path(file = file, ext = ".gpx")
  
  .check_extra_args(...)
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[[time_col]] <- paste0(format(x[[time_col]], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  if(is.null(color_col)) {
    color_col <- "color"
    x$color <- "Black"
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  
  make_lines <- function(x, time_col, name_col, description_col, color_col) {
    
    coords <- sf::st_coordinates(x[['geometry']])
    
    n_segments <- unique(coords[, 3])
    
    out <- character()
    for(ii in 1:length(n_segments)) {
      sel_coords <- coords[coords[, 3] == ii, ]
      
      # Tags formatted for OpenCPN using https://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd
      out <- paste0("<trk>\n",
                    "  <name>", x[name_col], "</name>\n",
                    "  <desc>", x[description_col], "</desc>\n",
                    "  <time>", x[time_col], "</time>\n",
                    "  <extensions>\n",
                    "    <gpxx:TrackExtension>\n        ",
                    "      <gpxx:DisplayColor>", x[color_col], "</gpxx:DisplayColor>\n",
                    "    </gpxx:TrackExtension>\n",
                    "  </extensions>",
                    "  <trkseg>",
                    paste(paste0("    <trkpt lat=\"", sel_coords[ , 2], "\" lon=\"", sel_coords[ , 1],"\"></trkpt>"), collapse = "\n"),
                    "  </trkseg>\n",
                    "</trk>")
      
    }
    
    out <- paste(out, collapse = "\n")
    
    return(out)
    
  }
  
  lines <- c("<?xml version=\"1.0\"?>",
             "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\" xmlns:gpxx=\"http://www.garmin.com/xmlschemas/GpxExtensions/v3\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www8.garmin.com/xmlschemas/GpxExtensionsv3.xsd\" xmlns:opencpn=\"http://www.opencpn.org\">",
             apply(X = x, 
                   MARGIN = 1, 
                   FUN = make_lines, 
                   time_col = time_col, 
                   name_col = name_col, 
                   description_col = description_col, 
                   color_col = color_col),
             "</gpx>")
  
  message("sf_to_gpx_track: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
}