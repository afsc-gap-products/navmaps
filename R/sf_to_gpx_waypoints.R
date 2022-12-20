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
#' @param software_format Character vector indicating which marine navigation software output should be formatted for.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_gpx_waypoints <- function(x, file, name_col, description_col, time_col = NULL, color_col, shape_col, software_format = "timezero", return_lines = FALSE) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col, shape_col))
  
  .check_valid_geometry(x = x, valid = c("POINT"))
  
  .check_output_path(file = file, ext = ".gpx")
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[time_col] <- paste0(format(x[time_col], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x[c('longitude', 'latitude')] <- sf::st_coordinates(x)
  x <- as.data.frame(x) |>
    dplyr::select(-geometry)
  

  stopifnot("sf_to_gpx: software_format must be timezero" = !(software_format %in% c("timzero")))
  stopifnot("sf_to_gpx: file extension must be .gpx"  = grepl(pattern = ".gpx", x = file))
  
  if(software_format == "timezero") {
    lines <- c("<?xml version=\"1.0\"?>",
               "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\">")
    
    for(ii in 1:nrow(x)) {
      lines <- c(lines, 
                 paste0("  <wpt lat=\"", x['latitude'][ii,], "\" lon=\"", x['longitude'][ii,], "\">"),
                 paste0("    <time>", x[time_col][ii,], "</time>"),
                 paste0("    <name>", x[name_col][ii,], "</name>"),
                 paste0("    <cmt>", x[description_col][ii,], "</cmt>"),
                 paste0("    <extensions>"),
                 paste0("      <MxTimeZeroSymbol>", x[shape_col][ii,], "</MxTimeZeroSymbol>"),
                 paste0("      <T0Color>", x[color_col][ii,], "</T0Color>"),
                 paste0("    </extensions>"),
                 paste0("  </wpt>")
      )
    }
    
    lines <- c(lines, "</gpx>")
    
  }
  
  message("sf_to_gpx: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
  if(return_lines) {
    
    return(lines)
    
  }
}
