#' Make track files from gps rds files
#' 
#' @param region Survey region as a character vector. One of "ai", "goa", "sebs", "nbs"
#' @param overwrite Should files be overwritten?

make_tracks <- function(region, overwrite = FALSE) {
  
  .check_region(x = region)
  
  raw_gps_paths <- list.files(here::here("output", region, "gps"), pattern = "raw_gps", full.names = TRUE)
  midpoint_paths <- gsub(pattern = "raw_gps", replacement = "midpoint", x = raw_gps_paths)
  
  # Calculating midpoints
  
  # Function to closest point to the midpoint
  calc_midpoint <- function(coords, dist) {
    coords[which.min(abs(max(dist)/2 - dist))]
  }
  
  for(ii in 1:length(raw_gps_paths)) {
    
    if(overwrite & file.exists(midpoint_paths[ii])) {
      message("Skipping file that already exists: ", midpoint_paths[ii])
      next
    } else{
      message("Processing ", raw_gps_paths[ii])
      
      # Smooth latitude and longitude, calculate elapsed time, and distance 
      temp_gps <- readRDS(raw_gps_paths[ii]) |>
        dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
        dplyr::arrange(DATE_TIME) |>
        tidyr::nest(df = c(LATITUDE, LONGITUDE, DATE_TIME)) |>
        dplyr::mutate(df = purrr::map(df, navmaps::smooth_lat_lon_dist)) |>
        tidyr::unnest(cols = df)
      
      # Find the point closest to the midpoint
      midpoint <-  temp_gps |>
        dplyr::group_by(VESSEL, CRUISE, HAUL) |>
        dplyr::summarise(midpoint = calc_midpoint(geometry, dist = DISTANCE_M))
      
      saveRDS(midpoint, file = midpoint_paths[ii])
    }
    
    # ALTERNATIVE METHOD FOR FINDING THE MIDPOINT OF TRACKS
    # gps_track <- temp_gps |>
    #   sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
    #                crs = "EPSG:4326") |>
    #   dplyr::group_by(VESSEL, CRUISE, HAUL) |>
    #   summarise(do_union = FALSE) |>
    #   sf::st_cast("LINESTRING") |>
    #   dplyr::group_by(VESSEL, CRUISE, HAUL) |>
    #   dplyr::mutate(utm_zone = navmaps::longitude_to_utm_zone(first_longitude(geometry))) # UTM zone for distance calcs
    
    # # Calculate midpoint points
    # unique_utm <- unique(gps_track$utm_zone)
    # 
    # for(ii in 1:length(unique_utm)) {
    #   
    #   sel_midpoint <- dplyr::filter(gps_track,
    #                                 utm_zone == unique_utm[ii]) |>  # Transform to UTM for distance calculations
    #     sf::st_transform(crs = paste0("+proj=utm +zone=", 
    #                                   unique_utm[ii], 
    #                                   " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) |>
    #     navmaps::st_line_midpoints() |>
    #     sf::st_transform(crs = "EPSG:4326") # Transform back to WGS84
    #   
    #   if(ii == 1) {
    #     midpoint <- sel_midpoint
    #   } else {
    #     midpoint <- dplyr::bind_rows(sel_midpoint, midpoint)
    #   }
    # }
    
  }
  
}






# pdf(here::here("plots", "simplify_track.pdf"), onefile = TRUE)
# for(jj in 1:nrow(gps_track)) {
#   
#   print(
#   ggplot() +
#     geom_sf(data = gps_track[jj,]) +
#     geom_sf(data = sf::st_simplify(gps_track[jj,], dTolerance = 20),
#             color = "red") +
#   geom_sf(data = sf::st_simplify(gps_track[jj,], dTolerance = 50),
#           color = "blue")
#   )
#   
# }
# dev.off()




# st_line_smooth <- function(sf_lines, spar = NULL) {
#   
#   .check_valid_geometry(sf_lines, valid = c("LINESTRING", "MULTILINESTRING"))
#   
#   g <- sf::st_geometry(sf_lines)
#   
#   g_smooth <- lapply(g, function(x) {
#     
#     coords <- as.matrix(x)
#     
#     dist <- c(0, cumsum(sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))))
#     
#     lon_spline <- smooth.spline(x = dist, y = coords[, 1], spar = spar)
#     lat_spline <- smooth.spline(x = dist, y = coords[, 2], spar = spar)
#     
#     smoothed <- sf::st_linestring(cbind(predict(lon_spline)$y,
#                                         predict(lat_spline)$y))
#     
#   }
#   )
#   
#   geometry <- sf::st_sfc(g_smooth, 
#                          crs = sf::st_crs(sf_lines))
#   
#   out <- sf::st_sf(geometry) |>
#     dplyr::bind_cols(as.data.frame(sf_lines) |>
#                        dplyr::select(-geometry))
#   
#   return(out)
#   
# }
# 
# 
# # Create midpoint points
# unique_utm <- unique(gps_track$utm_zone)
# 
# pdf(here::here("plots", "simplify_track.pdf"), onefile = TRUE)
# 
# for(ii in 1:length(unique_utm)) {
#   
#   raw_path <- dplyr::filter(gps_track,
#                             utm_zone == unique_utm[ii])
#   
#   smooth_path_09 <- raw_path |>  # Transform to UTM for distance calculations
#     sf::st_transform(crs = paste0("+proj=utm +zone=", 
#                                   unique_utm[ii], 
#                                   " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) |>
#     st_line_smooth(spar = 0.9) |>
#     sf::st_transform(crs = "EPSG:4326")
#   
#   smooth_path_08 <- raw_path |>  # Transform to UTM for distance calculations
#     sf::st_transform(crs = paste0("+proj=utm +zone=", 
#                                   unique_utm[ii], 
#                                   " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) |>
#     st_line_smooth(spar = 0.8) |>
#     sf::st_transform(crs = "EPSG:4326")
#   
#   for(jj in 1:nrow(raw_path)) {
#     
#     print(
#       ggplot() +
#         geom_sf(data = raw_path[jj,]) +
#         geom_sf(data = smooth_path_09[jj,],
#                 color = "blue") +
#         geom_sf(data = smooth_path_08[jj,],
#                 color = "deepskyblue2") +
#         ggtitle(paste0(raw_path$VESSEL[jj], " ", raw_path$CRUISE[jj], " ", raw_path$HAUL[jj]))
#     )
#     
#   }
#   
# }
# dev.off()


#' Internal function to calculate smooth latitude and longitude and calculate distance and time elapsed
#' 
#' Multiple steps in this function saves computing time converting between coordinate reference systems.
#' 
#' @param df data.frame of GPS data containing LONGITUDE, LATITUDE, and DATE_TIME columns
#' @param spar Smoothing parameter argument passed to smooth.spline()
#' @keywords internal
#' @export

smooth_lat_lon_dist <- function(df, spar = NULL) {
  
  # Detect UTM zone
  utm_zone <- navmaps::longitude_to_utm_zone(df$LONGITUDE[1])
  utm_crs <- paste0("+proj=utm +zone=", utm_zone, " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # Convert object to UTM for distance calculations
  sf_obj <- sf::st_as_sf(df, 
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = "EPSG:4326") |>
    sf::st_transform(crs = utm_crs)
  
  coords <- sf::st_coordinates(sf_obj)
  
  # Calculate time elapsed
  elapsed_sec <- cumsum(c(0, difftime(sf_obj $DATE_TIME[2:nrow(sf_obj )], sf_obj $DATE_TIME[1:(nrow(sf_obj)-1)])))
  
  # Correct GPS error using splines on latitude and longitude with time elapsed a predictor
  lon_spline <- smooth.spline(x = elapsed_sec, y = coords[, 'X'], spar = spar)
  lat_spline <- smooth.spline(x = elapsed_sec, y = coords[, 'Y'], spar = spar)
  
  # Add predictions and elapsed time back to the data frame
  smoothed_df <- tidyr::tibble(LONGITUDE = predict(lon_spline)$y,
                               LATITUDE = predict(lat_spline)$y,
                               DATE_TIME = df$DATE_TIME,
                               ELAPSED_SEC = elapsed_sec) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
                 crs = utm_crs)
  
  # Calculate distance between points in meters
  dist_sf <- smoothed_df |>
    dplyr::mutate(dist = sf::st_distance(geometry, dplyr::lag(geometry), by_element = TRUE))
  dist_sf$dist[1] <- 0
  
  # Cumulative distance travelled in meters
  smoothed_df$DISTANCE_M <- cumsum(as.numeric(dist_sf$dist))
  
  # Convert back to WGS84
  smoothed_df <- smoothed_df |>
    sf::st_transform(crs = "EPSG:4326") |>
    dplyr::mutate(LONGITUDE = sf::st_coordinates(geometry)[, 'X'],
                  LATITUDE = sf::st_coordinates(geometry)[, 'Y']) |>
    tibble::as_tibble()
  
  return(smoothed_df)
  
}
