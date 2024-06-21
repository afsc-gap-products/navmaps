# Make approximate area swept polygons ----

library(navmaps)
library(lubridate)
library(akgfmaps)
library(ggthemes)

channel <- navmaps::get_connected(schema = "AFSC")

# Average values from Somerton et al. (2007)
region <- "ai"
min_cruise <- 199100

contact_width <- c(16.1, 31.6, 47.8)
contact_type <- c("net", "bridles", "doors")

hauls <- RODBC::sqlQuery(channel = channel,
                         query = paste0("SELECT 
                                  VESSEL, 
                                  CRUISE, 
                                  HAUL, 
                                  STATIONID, 
                                  STRATUM, 
                                  NET_WIDTH, 
                                  PERFORMANCE,
                                  START_TIME,
                                  HAUL_TYPE
                               FROM 
                                  RACEBASE.HAUL 
                               WHERE 
                               REGION = '", toupper(region), "'")
)


start_end <- readRDS(file = here::here("output", region, paste0(region, "_haul_start_end.rds"))) |>
  dplyr::select(VESSEL, CRUISE, HAUL, DATE_TIME, EVENT) |>
  tidyr::pivot_wider(names_from = "EVENT", values_from = "DATE_TIME")

# Offset race_data timestamps to AKDT
start_end$start[start_end$CRUISE >= 200601] <- start_end$start[start_end$CRUISE >= 200601] - 8 * 3600
start_end$end[start_end$CRUISE >= 200601] <- start_end$end[start_end$CRUISE >= 200601] - 8 * 3600

fpath <- list.files(path = here::here("output", region, "gps"), 
                    full.names = TRUE,
                    pattern = "raw_gps")

trawl_width <- vector(mode = "list", length = 0L)

file.remove(here::here("output", region, "gps", 
                       paste0(region, "_buffered_towpath.gpkg")))

for(jj in 1:length(contact_width)) {
  
  idx <- 1 + (jj-1) * length(fpath)
  
  for(ii in 1:length(fpath)) {
    
    cat(fpath[ii], "\n")
    
    raw_gps <- readRDS(fpath[ii])
    
    if(!all(raw_gps$CRUISE > 199100)) {
      next
    }
    
    if(!("LONGITUDE" %in% names(raw_gps))) {
      next
    }
    
    raw_gps <- raw_gps |>
      dplyr::inner_join(start_end, by = c("VESSEL", "CRUISE", "HAUL"))
    
    smoothed_gps <- raw_gps |>
      dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE), DATE_TIME >= start, DATE_TIME <= end) |>
      dplyr::arrange(DATE_TIME) |>
      tidyr::nest(df = c(LATITUDE, LONGITUDE, DATE_TIME)) |>
      dplyr::mutate(df = purrr::map(df, navmaps::smooth_lat_lon_dist, spar = 0.9)) |>
      tidyr::unnest(cols = df) |>
      sf::st_as_sf() |>
      sf::st_transform(crs = "EPSG:3338")
    
    gps_lines <- smoothed_gps |>
      dplyr::arrange(DATE_TIME) |>
      dplyr::select(VESSEL, CRUISE, HAUL) |>
      dplyr::group_by(VESSEL, CRUISE, HAUL) |>
      dplyr::summarize(do_union = FALSE) |> 
      sf::st_cast("LINESTRING") |>
      dplyr::left_join(hauls)
    
    gps_buffered <- sf::st_buffer(gps_lines, 
                                  dist = contact_width[jj], 
                                  endCapStyle = "SQUARE")
    
    gps_buffered$contact_type <- contact_type[jj]
    
    trawl_width[[idx]] <- gps_buffered
    
    sf::st_write(gps_buffered, 
                 dsn = here::here("output", region, "gps", 
                                  paste0(region, "_buffered_towpath.gpkg")),
                 append = ifelse(idx == 1 & jj == 1, FALSE, TRUE))
    
    idx <- idx + 1
    
  }
  
}