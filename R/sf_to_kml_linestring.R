#' Write sf data to a kml linestring file
#' 
#' Write sf LINESTRING geometries to a kml linestring file
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param color_col Name of the column containing eight-digit hex color.
#' @param software_format Character vector indicating which marine navigation software output should be formatted for.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_kml_linestring <- function(x, file, name_col, description_col, color_col, software_format = "timezero", return_lines = FALSE) {
  
  .check_cols_exist(x = x, var_cols = c(name_col, description_col, color_col))
  
  .check_valid_geometry(x = x, valid = c("LINESTRING", "MULTIPOLYGON", "POLYGON"))
  
  .check_output_path(file = file, ext = ".kml")
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x_df <- as.data.frame(x)
  
  if(software_format == "timezero") {
    sel_color <- c(0, tz_pal(11))[x[[color_col]]+1]
  }
  
  lines <- c("<?xml version=\"1.0\" encoding=\"utf-8\"?>",
    "<kml xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns=\"http://www.opengis.net/kml/2.2\">",
    "  <Document>")
  
  if(all(st_geometry_type(x) == "LINESTRING")) {
    
    for(ii in 1:nrow(x)) {
      
      coords_df <- as.data.frame(sf::st_coordinates(x$geometry[ii]))
      
      
      
      
      unique_objects <- dplyr::select(coords_df, L1) |>
        unique()
      
      for(jj in 1:nrow(unique_objects)) {
        
        coords_sel <- dplyr::filter(coords_df, 
                                    L1 == unique_objects$L1[jj])
        
        coords_vec <- paste(
          coords_sel[['X']],
          coords_sel[['Y']], 
          sep = ","
        )
        
        lines <- c(lines, 
                   "    <Placemark>",
                   paste0("      <name>", x_df[name_col][ii,], "</name>"),
                   paste0("      <description>", x_df[description_col][ii,], "</description>"),
                   "      <LineString>",
                   "        <coordinates>",
                   paste(coords_vec, collapse = " \n        "),
                   "        </coordinates>",
                   "      </LineString>",
                   "      <Style>",
                   "        <LineStyle>",
                   paste0("        <color>", sel_color[ii], "</color>"),
                   "        </LineStyle>",
                   "      </Style>",
                   "    </Placemark>")
      }
    }
  } else {
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
                   "      <LineString>",
                   "        <coordinates>",
                   paste(coords_vec, collapse = " \n        "),
                   "        </coordinates>",
                   "      </LineString>",
                   "      <Style>",
                   "        <LineStyle>",
                   paste0("        <color>", sel_color[ii], "</color>"),
                   "        </LineStyle>",
                   "      </Style>",
                   "    </Placemark>")
      }
    }
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