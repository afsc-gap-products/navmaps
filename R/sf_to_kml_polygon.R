#' Write sf data to a kml polygon file
#' 
#' Write sf POLYGON or MULTIPOLYGON geometries to a kml polygon file.
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param time_col Time column name. Optional. If not provided, writes the system time.
#' @param color_col Name of the column containing eight-digit hex color.
#' @param fill_col Name of the column containing eight-digit hex color. 
#' @param shape_col Name of the column containing integer shapes.
#' @param software_format Character vector indicating which marine navigation software output should be formatted for.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_kml_polygon <- function(x, file, name_col, description_col, time_col = NULL, color_col, fill_col, software_format = "timezero", return_lines = FALSE) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col, fill_col))
  
  .check_valid_geometry(x = x, valid = c("POLYGON", "MULTIPOLYGON"))
  
  .check_output_path(file = file, ext = ".kml")
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x_df <- as.data.frame(x)

  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
             "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
             "  <Document>")
  
  for(ii in 1:nrow(x)) { 
    
    coords_df <- as.data.frame(sf::st_coordinates(x$geometry[ii]))
    
    unique_objects <- dplyr::select(coords_df, L1, L2) |>
      unique()
    
    for(jj in 1:nrow(unique_objects)) {
      
     coords_sel <- dplyr::filter(coords_df, 
                                 L1 == unique_objects$L1[jj],
                                 L2 == unique_objects$L2[jj])
     
     coords_vec <- paste(
       coords_sel[['X']],
       coords_sel[['Y']], 
       sep = ","
     )
      
      lines <- c(lines, 
                 "    <Placemark>",
                 paste0("      <name>", x_df[name_col][ii,], "</name>"),
                 paste0("      <description>", x_df[description_col][ii,], "</description>"),
                 paste0("      <time>", x_df[time_col][ii,], "</time>"),
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
                 "              <coordinates>",
                 paste(coords_vec, collapse = " \n        "),
                 "              </coordinates>",
                 "            </LinearRing>",
                 "          </outerBoundaryIs>",
                 "        </Polygon>",
                 "      </MultiGeometry>",
                 "    </Placemark>")
    }

  }
    
    lines <- c(lines,
               "  </Document>",
               "</kml>")
  
  message("sf_to_kml_polygon: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
  if(return_lines) {
    return(lines)
  }
  
  
}