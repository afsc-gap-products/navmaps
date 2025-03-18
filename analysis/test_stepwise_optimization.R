library(navmaps)


max_daily_hr = 12
processing_time_hr = 1.5
max_daily_stn = 6
transit_speed_kmh = 1.852*7 # Converted from knots to km/h
set_retrieve_hr = 0.5
set_on_arrival = FALSE

station_pool  <- 
  sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::filter(VESSEL == 148)

sf::st_geometry(station_pool) <- "geometry" 

# station_pool <- 
#   station_pool[sf::st_coordinates(station_pool)[, 1] < -200000, ]

station_cluster <- station_pool[which.min(sf::st_coordinates(station_pool)[, 1]), ]
station_cluster$node <- 1
station_cluster$distance <- 0

output <- vector(mode = "list", length = 6)
day <- 1

while(nrow(station_pool) > 0 & day < 60) {
  
  for(ii in 1:7) {
      
      sub_stations <- 
        dplyr::anti_join(
          station_pool,
          sf::st_drop_geometry(station_cluster),
          by = c("STATION", "GRIDID", "NMFS_AREA", "REP_AREA", "STRATUM", "TRAWLABLE", "AREA_KM2", "x", "y", "LONGITUDE", "LATITUDE", "VESSEL")
        )
      
      if(nrow(sub_stations) > 0) {
        
        if(ii > 1) {
          
          rank_distance <- rank(
            sf::st_distance(
              station_cluster[ii-1, ], 
              sub_stations)
          )
          
          station_cluster <- dplyr::bind_rows(
            station_cluster, 
            sub_stations[rank_distance == 1, ]
          )
          
          station_cluster$distance[ii] <- 
            sf::st_distance(station_cluster[ii, ],
                            station_cluster[ii-1, ]) |>
            as.numeric()/1e3
          station_cluster$node[ii] <- ii
          
        }
      }
    
    if(ii == 7) {
      
      station_cluster_geom <- sf::st_geometry(station_cluster)
      
      station_cluster <- planning_calc_survey_days(
        station_nodes = station_cluster,
        max_daily_hr = max_daily_hr,
        processing_time_hr = processing_time_hr,
        max_daily_stn = max_daily_stn,
        transit_speed_kmh = transit_speed_kmh,
        set_retrieve_hr = set_retrieve_hr,
        set_on_arrival = TRUE
      )$station_nodes
      
      station_cluster$day <- station_cluster$day + day - 1
      
      sf::st_geometry(station_cluster) <- station_cluster_geom
      
      station_day <- station_cluster[station_cluster$day == day, ]
      
      if(nrow(station_day) >= 6) {
        station_day <- station_day[1:6, ]
      }
      
      station_pool <-
        dplyr::anti_join(
          station_pool,
          sf::st_drop_geometry(station_day),
          by = c("STATION", "GRIDID", "NMFS_AREA", "REP_AREA", "STRATUM", "TRAWLABLE", "AREA_KM2", "x", "y", "LONGITUDE", "LATITUDE", "VESSEL")
        )
      
      if(day == 1) {
        
        stations_completed <- station_day
        
      } else {
        
        stations_completed <- 
          dplyr::bind_rows(
            stations_completed,
            station_day
          )
        
      }
      
      if(nrow(station_day) >= 6) {
        station_cluster <- station_cluster[7, ]
      } else {
        station_cluster <- station_cluster[station_cluster$day != day, ][1, ]
      }
      
      if(!(nrow(station_pool) > 0)) {
        next
      }
      
      if(try(sf::st_coordinates(station_cluster)[1, 1]) - min(sf::st_coordinates(station_pool)[, 1]) > 1e5 || is.na(sf::st_coordinates(station_cluster)[1, 1])) {
        station_cluster <- station_pool[which.min(sf::st_coordinates(station_pool)[, 1]), ]
      }

      station_cluster$distance <- 0
      station_cluster$node <- 1
      station_cluster$hours_elapsed <- 1
      
      day <- day + 1
      
    }
    
  }
  
  print(paste(day, " ", nrow(station_pool)))
  
}

sf::st_write(obj = stations_completed,
             dsn = here::here("analysis", "goa_2025_tsp", "test_stations_completed.gpkg"),
             delete_dsn = TRUE)
