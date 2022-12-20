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
#' @param software_format Character vector indicating which marine navigation software output should be formatted for.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_kml_points <- function(x, file, name_col, description_col, shape_col = NULL, time_col = NULL, color_col, software_format = "timezero", return_lines = FALSE) {
  
  # library(navmaps)
  # region <- "ai"
  # 
  # # Load shapefiles using the akgfmaps package
  # map_layers <- akgfmaps::get_base_layers(select.region = region)
  # grid_centers <- sf::st_centroid(map_layers$survey.grid) # Points at the center of each grid cell
  # grid_centers$shape <- 3
  # grid_centers$color <- tz_pal(values = "yellow")
  # 
  #   x = grid_centers
  #   file = here::here("output", region, "navigation", paste0(region, "_marks.kml"))
  #   name_col = "ID"
  #   description_col = "STRATUM"
  #   color_col = "color"
  #   shape_col = "shape"
  #   time_col = NULL
  #   software_format = "timezero"
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col))
  
  .check_valid_geometry(x = x, valid = c("POINT"))
  
  .check_output_path(file = file, ext = ".kml")
  
  if(is.null(time_col)) {
    time_col <- "time"
    x$time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  } else {
    x[time_col] <- paste0(format(x[time_col], "%Y-%m-%dT%H:%M:%S"), ".000000Z")
  }
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x_df <- cbind(as.data.frame(x), 
                as.data.frame(sf::st_coordinates(x$geometry)))
  
  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
             "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
             "  <Document>")
    
    for(ii in 1:nrow(x_df)) {
      
        lines <- c(lines, 
                   "    <Placemark>",
                   paste0("      <name>", x_df[name_col][ii,], "</name>"),
                   paste0("      <description>", x_df[description_col][ii,], "</description>"),
                   paste0("      <TimeStamp><when>", x_df[time_col][ii,], "</when></TimeStamp>"), # This doesn't work
                   "      <Point>",
                   "        <coordinates>",
                   paste0(x_df['X'][ii,], ",", x_df['Y'][ii,]),
                   "        </coordinates>",
                   "      </Point>",
                   "      <Style>",
                   "        <IconStyle>",
                   paste0("        <color>", x_df[color_col][ii,], "</color>"),
                   "        </IconStyle>",
                   "      </Style>",
                   "    </Placemark>")
      }
  
  lines <- c(lines,
             "  </Document>",
             "</kml>")
  
  message("sf_to_kml_linestring: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
  if(return_lines) {
    return(lines)
  }
  
}