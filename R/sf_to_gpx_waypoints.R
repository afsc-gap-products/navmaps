#' Write sf point data to a .gpx waypoint file
#' 
#' Used to convert data.frames into a gpx file that can be read into marine navigation software.
#' 
#' @param x data.frame containing latitude, longitude, name, color, and shape columns.
#' @param file Output file with a .gpx extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param color_col Name of the column containing integer colors.
#' @param shape_col Name of the column containing integer shapes.
#' @param format Character vector indicating which marine navigation software output should be formatted for.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_gpx_waypoints <- function(x, file, name_col, description_col, color_col, shape_col, format = "timezero", return_lines = FALSE) {
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x[c('longitude', 'latitude')] <- sf::st_coordinates(x)
  x <- as.data.frame(x) |>
    dplyr::select(-geometry)
  
  var_cols <- c(name_col, description_col, color_col, shape_col)
  missing_cols <- var_cols[which(!(var_cols %in% names(x)))]
  
  if(length(missing_cols) >=1) {
    stop("sf_to_gpx: The following variable columns were not found in x: ", missing_cols)
  }

  stopifnot("sf_to_gpx: format must be timezero" = !(format %in% c("timzero")))
  stopifnot("sf_to_gpx: file extension must be .gpx"  = grepl(pattern = ".gpx", x = file))
  
  if(format == "timezero") {
    lines <- c("<?xml version=\"1.0\"?>",
               "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\">")
    
    set_time = paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
    
    for(ii in 1:nrow(x)) {
      lines <- c(lines, 
                 paste0("  <wpt lat=\"", x['latitude'][ii,], "\" lon=\"", x['longitude'][ii,], "\">"),
                 paste0("    <time>", set_time, "</time>"),
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
  gpx_con <- file(file)
  
  writeLines(text = lines, 
             con = gpx_con)
  
  close(gpx_con)
  
  return(lines)
}
