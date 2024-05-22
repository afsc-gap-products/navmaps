library(navmaps)
library(lubridate)
library(akgfmaps)

channel <- navmaps::get_connected(schema = "AFSC")

# Average values from Somerton et al. (2007)
bridle_width <- 13.5
net_width <- 16.1 ## PLACEHOLDER
region <- "ai"
min_cruise <- 199100

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

fpath <- list.files(path = here::here("output", region, "gps"), 
                    full.names = TRUE,
                    pattern = "raw_gps")

trawl_width <- vector(mode = "list", length = 0L)

idx <- 1

for(ii in 1:length(fpath)) {
  
  cat(fpath[ii], "\n")
  
  raw_gps <- readRDS(fpath[ii])
  
  if(!all(raw_gps$CRUISE > 199100)) {
    next
  }
  
  if(!("LONGITUDE" %in% names(raw_gps))) {
    next
  }
  
  smoothed_gps <- readRDS(fpath[ii]) |>
    dplyr::filter(!is.na(LONGITUDE), !is.na(LATITUDE)) |>
    dplyr::arrange(DATE_TIME) |>
    tidyr::nest(df = c(LATITUDE, LONGITUDE, DATE_TIME)) |>
    dplyr::mutate(df = purrr::map(df, navmaps::smooth_lat_lon_dist, spar = 0.9)) |>
    tidyr::unnest(cols = df) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = "EPSG:3338")
  
  start_end <- readRDS(file = here::here("output", region, paste0(region, "_haul_start_end.rds")))
  
  start_end_year <- dplyr::filter(start_end, 
                                  lubridate::year(DATE_TIME) %in% unique(lubridate::year(smoothed_gps$DATE_TIME)),
                                  VESSEL %in% unique(smoothed_gps$VESSEL))
  
  gps_lines <- smoothed_gps |>
    dplyr::arrange(DATE_TIME) |>
    dplyr::select(VESSEL, CRUISE, HAUL) |>
    dplyr::group_by(VESSEL, CRUISE, HAUL) |>
    dplyr::summarize(do_union = FALSE) |> 
    sf::st_cast("LINESTRING") |>
    dplyr::left_join(hauls)
  
  gps_buffered <- sf::st_buffer(gps_lines, 
                                dist = bridle_width + net_width, 
                                endCapStyle = "SQUARE")
  
  trawl_width[[idx]] <- gps_buffered

  sf::st_write(gps_buffered, 
               dsn = here::here("output", region, "gps", 
                                paste0(region, "_buffered_towpath.gpkg")),
               append = ifelse(idx > 1, TRUE, FALSE))
  
  idx <- idx + 1
  
}
