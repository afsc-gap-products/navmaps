# Traveling salesman solution to survey time

# install.packages(c("sf", "TSP", "here", "dplyr"))

library(TSP)
library(here)
library(dplyr)
library(sf)

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.5 # Minimum processing time in hours
max_daily_stn = 4 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*9 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
vessel = 148

# Algorithm to calculate days ----
calc_days <- function(stn_plan,
                      max_daily_hr, 
                      processing_time_hr,
                      max_daily_stn, 
                      transit_speed_kmh,
                      set_retrieve_hr) {
  
  day <- 1
  hours_elapsed <- 0
  day_stations <- 0
  ii <- 1
  
  while(ii < nrow(stn_plan)) {

    
    transit_hours <- max(
      c(
        (processing_time_hr - set_retrieve_hr) + stn_plan$distance[ii+1] / transit_speed_kmh,
        processing_time_hr
      )
    )
    
    day_stations <- day_stations + 1
    
    if(transit_hours > max_daily_hr) {
      
      day <- day + 1
      hours_elapsed <- 0
      day_stations <- 0 
      ii <- ii + 1
      
    } else if(hours_elapsed + transit_hours > max_daily_hr & transit_hours < max_daily_hr || 
              max_daily_stn < day_stations) {
      
      day <- day + 1
      hours_elapsed <- 0
      day_stations <- 0 
      
    } else {
      
      hours_elapsed <- hours_elapsed + transit_hours
      ii <- ii + 1
      
    } 
    
  }
  
  return(day)
  
}

# Calculations with station allocation ----

# Minimum distance using the traveling salesman problem package (TSP)

# Ocean Explorer

# 2023 example
vessel_dist <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
  sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
  dplyr::filter(vessel == "AKP") |>
  sf::st_transform(crs = "EPSG:32606")
  
# 2025 design
# vessel_dist <- sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
# sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
# dplyr::filter(VESSEL == vessel) # OEX

vessel_tsp <- vessel_dist |>
  sf::st_distance() |>
  as.matrix() |> 
  matrix(
    nrow = nrow(vessel_dist), 
    ncol = nrow(vessel_dist)
    ) |>
  TSP::as.ATSP() |> 
  TSP::TSP() |> 
  TSP::solve_TSP(method = "nearest_insertion")

vessel_dist$order <- as.numeric(attr(vessel_tsp, "names"))
vessel_dist <- vessel_dist[as.numeric(attr(vessel_tsp, "names")), ]
vessel_dist$distance <- c(0,
                       sf::st_distance(x = vessel_dist[1:(nrow(vessel_dist)-1), ], 
                                       y = vessel_dist[2:nrow(vessel_dist), ], 
                                       by_element = TRUE)
)

vessel_dist$distance <- as.numeric(vessel_dist$distance / 1000)

calc_days(
  stn_plan = sf::st_drop_geometry(vessel_dist),
  max_daily_hr = max_daily_hr, 
  processing_time_hr = processing_time_hr,
  max_daily_stn = max_daily_stn, 
  transit_speed_kmh = transit_speed_kmh,
  set_retrieve_hr = set_retrieve_hr
)

ggplot() +
  geom_sf_text(data = vessel_dist,
          mapping = aes(label = order))
