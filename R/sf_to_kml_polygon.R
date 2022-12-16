#' Convert sf to kml polygons
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param kml_file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param color_col Name of the column containing eight-digit hex color.
#' @param fill_col Name of the column containing eight-digit hex color. 
#' @param shape_col Name of the column containing integer shapes.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export
#' 

sf_to_kml_polygon <- function(x, kml_file, name_col, description_col, color_col, fill_col, return_lines = FALSE) {
  
  var_cols <- c(name_col, description_col, color_col, fill_col)
  missing_cols <- var_cols[which(!(var_cols %in% names(x)))]
  
  if(length(missing_cols) >=1) {
    stop("sf_to_kml_polygon: The following variable columns were not found in x: ", missing_cols)
  }
  
  stopifnot("sf_to_kml_polygon: kml_file extension must be .kml"  = grepl(pattern = ".kml", x = kml_file))
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x_df <- as.data.frame(x)
  
  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
             "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
             "  <Document>")
  
  for(ii in 1:nrow(x)) { 
    
    coords_vec <- paste(
      sf::st_coordinates(x$geometry[ii])[,'X'],
      sf::st_coordinates(x$geometry[ii])[,'Y'], sep = ","
    )
    
    lines <- c(lines, 
               "    <Placemark>",
               paste0("      <name>", x_df[name_col][ii,], "</name>"),
               paste0("      <description>", x_df[description_col][ii,], "</description>"),
               "      <Style>",
               "        <LineStyle>",
               paste0("        <color>", x_df[color_col][ii,], "</color>"),
               "        </LineStyle>",
               "        <PolyStyle>",
               paste0("        <color>", x_df[fill_col][ii,], "</color>"),
               "        </PolyStyle>",
               "      </Style>",
               "      <MultiGeometry>",
               "        <Polygon>",
               "          <outerBoundaryIs>",
               "            <LinearRing>",
               "              <coordinates>")
    for(jj in 1:length(coords_vec)) {
      lines <- c(lines,
                 paste0("        ", coords_vec[jj])
      )
    }
    
    lines <- c(lines, 
               "              </coordinates>",
               "            </LinearRing>",
               "          </outerBoundaryIs>",
               "        </Polygon>",
               "      </MultiGeometry>",
               "    </Placemark>")
  }
    
    lines <- c(lines,
               "  </Document>",
               "</kml>")
  
  message("sf_to_gpx: Writing ", length(lines), " lines to ", kml_file)
  kml_con <- file(kml_file)
  
  writeLines(text = lines, 
             con = kml_con)
  
  close(kml_con)
  
  if(return_lines) {
    return(lines)
  }
  
  
}