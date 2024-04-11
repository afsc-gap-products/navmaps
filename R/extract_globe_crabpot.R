#' Extract crab pot lines from Globe
#' 
#' This function extracts linestrings and points from Globe formatted data and writes POINT and LINESTRING geometries to a Geopackage or Geodatabase file for subsequent review and editing. Alternatively accepts linestings and points as sf objects.
#' 
#' @param globe_file Path to a Globe .mdb or .accdb file, or Globe-formatted text file (.xlsx, .xls, .csv). Provide this or raw_lines and raw_points as sf objects.
#' @param raw_lines Converted linestrings as an sf object.
#' @param raw_points Converted points as an sf objects.
#' @param output_file Output filepath. Written to default file path if not provided.
#' @param vessel_name Required. Name of the vessel as a character vector.
#' @param region Region as a character vector.
#' @param file_format Geopackage "gpkg" or geodatabase "gdb"
#' @export

extract_globe_crabpot <- function(globe_file = NULL, raw_lines = NULL, raw_points = NULL, output_file = NULL, vessel_name, region = NULL, 
                                     file_format = "gpkg") {
  
  file_format <- tolower(file_format)
  
  stopifnot("make_crabpot_raw: File format must be gdb or gpkg." = !is.null(globe_file) & file_format %in% c("gpkg", "gdb"))
  
  stopifnot("make_crabpot_raw: raw_lines and raw_points must be NULL if globe_file is provided." = 
              is.null(raw_lines) & is.null(raw_points) & !is.null(globe_file))
  
  vessel_name_short <- gsub(pattern = " ", 
                      replacement = "_", 
                      x = gsub(pattern = "[^[:alnum:] ]", 
                               replacement = "", 
                               x = vessel_name))
  
  if(is.null(output_file)) {
    output_file <- here::here("output", region, "shapefiles",  
                              paste0(
                                paste(c(region, "unedited_crabpot_"), collapse = "_"),
                                vessel_name_short,
                                ".", 
                                file_format))
    
  }
  
  navmaps::.check_output_path(output_file)
  
  raw_lines <- navmaps::globe_to_sf(dsn = globe_file, 
                                    keep_index = TRUE)
  
  raw_lines$vessel <- vessel_name
  
  raw_points <- navmaps::globe_to_sf(dsn = globe_file, 
                                     wkt_geometry_type = "POINT",
                                     keep_index = TRUE)
  
  raw_points$vessel <- vessel_name
  
  raw_points$intersects <- as.numeric(sf::st_intersects(raw_points, raw_lines, sparse = TRUE))
  
  stray_points <- dplyr::filter(raw_points, is.na(intersects))
  
  message("make_crabpot_raw: Writing raw lines, raw points, and stray points to ", output_file)
  
  if(file.exists(output_file)) {
    file.remove(output_file)
  }
  
  sf::st_write(obj = raw_lines, layer = paste0(vessel_name_short, "_raw_lines"), dsn = output_file, delete_layer = TRUE, append = FALSE)
  sf::st_write(obj = raw_points, layer = paste0(vessel_name_short, "_raw_points"), dsn = output_file, delete_layer = TRUE, append = TRUE)
  sf::st_write(obj = stray_points, layer = paste0(vessel_name_short, "_stray_points"), dsn = output_file, delete_layer = TRUE, append = TRUE)
  
}