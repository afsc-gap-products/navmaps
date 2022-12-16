#' Write sf to gpx route file
#' 
#' Write sf LINESTRING, POLYGON, MULTIPOLYGON, or MULTILINESTRING geometries to a gpx file
#' 
#' @param x sf object that contains a linestring geometry and fields with name, description, and color.
#' @param file Output file with a .kml extension.
#' @param name_col Name of the column containing names.
#' @param description_col Description column.
#' @param return_lines Should lines written to gpx file also be returned by the function. Used for debugging.
#' @export

sf_to_gpx_track  <- function(x, file, name_col, description_col, return_lines = FALSE) {

  stopifnot("sf_to_gpx_track: x must contain only LINESTRING, POLYGON, MULTIPOLYGON, or MULTILINESTRING geometries" = all(st_geometry_type(x) %in% c("LINESTRING", "POLYGON", "MULTIPOLYGON", "MULTILINESTRING")))
  
  var_cols <- c(name_col, description_col)
  missing_cols <- var_cols[which(!(var_cols %in% names(x)))]
  
  if(length(missing_cols) >=1) {
    stop("sf_to_gpx_track: The following variable columns were not found in x: ", missing_cols)
  }
  
  stopifnot("sf_to_gpx_track: file extension must be .kml"  = grepl(pattern = ".gpx", x = file))
  
  x <- sf::st_transform(x, crs = "EPSG:4326")
  x_df <- as.data.frame(x)
  
  lines <- c("<?xml version=\"1.0\"?>",
             "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" version=\"1.1\" xmlns=\"http://www.topografix.com/GPX/1/1\">")
    
    if(all(st_geometry_type(x) == "LINESTRING")) {
      
      for(ii in 1:nrow(x)) {
        
        coords_df <- as.data.frame(sf::st_coordinates(x$geometry[ii]))
        
        
        unique_objects <- dplyr::select(coords_df, L1) |>
          unique()
        
        
        for(jj in 1:nrow(unique_objects)) {
          
          coords_sel <- dplyr::filter(coords_df, 
                                      L1 == unique_objects$L1[jj])
          
          lines <- c(lines, 
                     "<trk>",
                     paste0("  <name>", x_df[name_col][ii,], "</name>"),
                     paste0("  <desc>", x_df[description_col][ii,], "</desc>"),
                     "  <trkseg>")
          
          for(kk in 1:nrow(coords_sel)) {
            lines <- c(lines,
                       paste0("<trkpt lat=\"", coords_sel[['Y']][kk], "\" lon=\"", coords_sel[['X']][kk],"\"></trkpt>")
            )
          }
          
          lines <- c(lines, 
                     "</trkseg>",
                     "</trk>")
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
          
          lines <- c(lines, 
                     "<trk>",
                     paste0("  <name>", x_df[name_col][ii,], "</name>"),
                     paste0("  <desc>", x_df[description_col][ii,], "</desc>"),
                     "  <trkseg>")
          
          for(kk in 1:nrow(coords_sel)) {
            lines <- c(lines,
                       paste0("<trkpt lat=\"", coords_sel[['Y']][kk], "\" lon=\"", coords_sel[['X']][kk],"\"></trkpt>")
            )
          }
          
          lines <- c(lines, 
                     "</trkseg>",
                     "</trk>")
        }
      }
    }
  
  lines <- c(lines,
             "</gpx>")
  
  message("sf_to_gpx_track: Writing ", length(lines), " lines to ", file)
  con <- file(file)
  
  writeLines(text = lines, 
             con = con)
  
  close(con)
  
  if(return_lines) {
    return(lines)
  }
  
}
