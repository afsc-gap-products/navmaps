#' Convert sf to navigation file
#'
#' This function uses the sf geometry type and software_format arguments to choose an sf_to_[output_type] for writing spatial data to a file formatted for marine navigation software. For example, if the sf object passed to x contains POINT geometries and the software_format is set to "globe", the function calls sf_to_globe_points() to write data to file (.mdb, .accdb, or .csv, depending on the extension for 'file'). If the sf object passed to x contains POINT geometries and the software_format is set to "opencpn" or "timezero", the function calls sf_to_gpx_points() to write data to a .gpx file.
#'
#' @param x sf object
#' @param file output file name
#' @param software_format "globe", "timezero", or "opencpn"
#' @param geometry Optional. Type of sf geometry as a character ("POINT", "LINESTRING",  "POLYGON", "MULTIPOINT", "MULTISTRING", "MULTIPOLYGON"). If NULL, the most common geometry type in the sf object is used.
#' @param color_col Column name passed to sf_to_[output_type] functions.
#' @param shape_col Column name passed to sf_to_[output_type] functions. Only used for POINTS and MULTIPOINT geometries.
#' @param fill_col Column name passed to sf_to_[output_type] functions. Only used for .kml polygons (i.e., boundaries).
#' @param time_col Column name passed to sf_to_[output_type] functions.
#' @param name_col Column name passed to sf_to_[output_type] functions. Only used for .gpx and .kml files.
#' @param description_col Column name passed to sf_to_[output_type] functions. Only used for .gpx and .kml files.
#' @param depth_col Column name passed to sf_to_[output_type] functions. Only used for Globe tracks created from POINT and MULTIPOINT geometries.
#' @param index_col Column name passed to sf_to_[output_type] functions. Only used for Globe tracks created from POINT and MULTIPOINT geometries.
#' @param extra_cols Column names passed to sf_to_[output_type] functions. Only used for Globe .csv, .mdb, and .accdb files.
#' @param tracks Logical. Should the output be a track file?
#' @param envir Only change for debugging.
#' @export

sf_to_nav_file <- function(x, 
                           file, 
                           software_format, 
                           geometry = NULL, 
                           color_col = NULL, 
                           shape_col = NULL, 
                           fill_col = NULL, 
                           time_col = NULL, 
                           name_col = NULL, 
                           description_col = NULL, 
                           extra_cols = NULL, 
                           depth_col = NULL, 
                           index_col = NULL, 
                           tracks = FALSE, 
                           envir = environment()) {
  
  args <- as.list(match.call()[-1])
  
  software_format <- tolower(software_format)
  
  .check_software(software_format)
  
  valid_geometry <- c("POINT", "LINESTRING",  "POLYGON", "MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON")
  
  if(is.null(geometry)) {
    geometry <- detect_geometry_type(x)
  } else {
    geometry <- toupper(geometry)
    stopifnot(geometry %in% valid_geometry)
  }
  
  .check_valid_geometry(x, valid_geometry)
  
  is_point <- geometry %in% c("POINT", "MULTIPOINT")
  is_linestring <- geometry %in% c("LINESTRING", "MULTILINESTRING")

  if(software_format == "globe") {
    
    if(!tracks) {
      x$Name <- x[[name_col]]
      x$Comment <- x[[description_col]]
    }

    if(is_point & tracks) {
        message("sf_to_nav_file: Using sf_to_tracks()")
        do.call(sf_to_tracks, args = args, envir = parent.frame())
      } 
    
    if(is_point & !tracks) {
        message("sf_to_nav_file: Using sf_to_globe_points()")
        do.call(sf_to_globe_points, args = args, envir = parent.frame())
      }

    if(!is_point) {
      message("sf_to_nav_file: Using sf_to_globe_linestring()")
      do.call(sf_to_globe_linestring, args = args, envir = parent.frame())
    }
    
  }
  
  if(software_format == "timezero") {
    
    if(tracks) {
      message("sf_to_nav_file: Using sf_to_kml_track()")
      do.call(sf_to_kml_track, args = args, envir = parent.frame())
    }
    
    if(is_point & !tracks) {
      message("sf_to_nav_file: Using sf_to_gpx_waypoints()")
      do.call(sf_to_gpx_waypoints, args = args, envir = parent.frame())
    }
    
    if(is_linestring & !tracks) {
      message("sf_to_nav_file: Using sf_to_kml_linestring()")
      do.call(sf_to_kml_linestring, args = args, envir = parent.frame())
    }
    
    if(!is_point & !is_linestring & !tracks) {
      message("sf_to_nav_file: Using sf_to_kml_polygon()")
      do.call(sf_to_kml_polygon, args = args, envir = parent.frame())
    }
    
  }

  if(software_format == "opencpn") {
    
    if(is_point) {
      message("sf_to_nav_file: Using sf_to_gpx_waypoints()")
      do.call(sf_to_gpx_waypoints, args = args, envir = parent.frame())
    } else {
      message("sf_to_nav_file: Using sf_to_gpx_track()")
      do.call(sf_to_gpx_track, args = args, envir = parent.frame())
    }
    
  }

}