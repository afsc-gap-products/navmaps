# Traveling salesman solution to survey time

# install.packages(c("sf", "TSP", "here", "dplyr"))
library(navmaps)
library(TSP)
library(here)
library(dplyr)
library(sf)

#' Solve a traveling Salesman problem (TSP) for a survey station alaocation
#'
#' This function optimizes the sampling order of stations by solving a Traveling Salesman Problem (TSP) using the nearest insertion method to minimize the distance traveled.
#'
#' @param x An `sf` object representing the survey stations.
#'
#' @return A list containing:
#' \item{distance_nodes}{An `sf` object of survey stations ordered based on 
#' the optimal TSP solution, with an added column for inter-station distances in kilometers.}
#' \item{tsp}{The TSP solution object.}
#'
#' @importFrom sf st_distance st_transform
#' @importFrom TSP as.ATSP TSP solve_TSP
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' # Load and transform survey stations
#' x <- system.file("extdata", "goa_station_allocation_520.shp", package = "navmaps") |> 
#' sf::st_read() |>
#'   sf::st_transform(crs = "EPSG:32606") |> # UTM zone 2
#'   dplyr::filter(VESSEL == 148) # Ocean Explorer
#' 
#' # Solve TSP for station order
#' tsp_out <- planning_solve_station_tsp(x = x)
#' 
#' # Estimate sampling days
#' survey_days <- planning_calc_survey_days(
#'   station_nodes = tsp_out$distance_nodes,
#'   max_daily_hr = 12,
#'   processing_time_hr = 1.5,
#'   max_daily_stn = 6,
#'   transit_speed_kmh = 1.852*7, # Converted from knots to km/h
#'   set_retrieve_hr = 0.5,
#'   set_on_arrival = FALSE
#' )
#' 
#' print(survey_days)
#' }
#'
#' @export
#'

planning_solve_station_tsp <- 
  function(x, hamilton = TRUE) {
    
    tsp_data <- x |>
      sf::st_distance() |>
      as.matrix() |> 
      matrix(
        nrow = nrow(x), 
        ncol = nrow(x)
      ) |>
      TSP::TSP()
    
    if (hamilton) {
      # Insert dummy city to hack the solver to calculate the hamiltonian path
      tsp_data <- TSP::insert_dummy(tsp_data, label = "cut")
    }
    
    tsp_solution <- TSP::solve_TSP(x = tsp_data,
                                   method = "nearest_insertion")
    
    if (hamilton) {
      # Insert dummy city to hack the solver to calculate the hamiltonian path
      tsp_solution <- TSP::cut_tour(tsp_solution, "cut")
    }
    
    x$node <- as.numeric(attr(tsp_solution, "names"))
    x <- x[as.numeric(attr(tsp_solution, "names")), ]
    x$order <- 1:nrow(x)
    x$distance <- c(0,
                    sf::st_distance(x = x[1:(nrow(x)-1), ], 
                                    y = x[2:nrow(x), ], 
                                    by_element = TRUE)
    )
    
    x$distance <- as.numeric(x$distance / 1000)
    
    return(list(distance_nodes = x, 
                tsp = tsp_solution)
    )
    
  }

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.25 # Minimum processing time in hours
max_daily_stn = 6 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*9 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
set_on_arrival = FALSE # Set on arrival at the station or set based on the minimum processing time?
vessel = 148

# Calculations with station allocation ----

# Minimum distance using the traveling salesman problem package (TSP)

# Ocean Explorer

# 2023 example
# vessel_dist <- read.csv(file = here::here("assets", "data", "allocation", "GOA2023_Station_allocation_520_EW.csv")) |>
#   sf::st_as_sf(crs = "WGS84", coords = c("longitude", "latitude")) |>
#   dplyr::filter(vessel == "OEX") |>
#   sf::st_transform(crs = "EPSG:32606")

# 2025 design
vessel_dist <- sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

vessel_tsp <- planning_solve_station_tsp(x = vessel_dist, 
                                         hamilton = TRUE)

station_cluster = vessel_tsp$distance_nodes
vessel_days <- 
  planning_calc_survey_days(
    station_nodes = vessel_tsp$distance_nodes,
    max_daily_hr = max_daily_hr, 
    processing_time_hr = processing_time_hr,
    max_daily_stn = max_daily_stn, 
    transit_speed_kmh = transit_speed_kmh,
    set_retrieve_hr = set_retrieve_hr,
    set_on_arrival = TRUE
  )

vessel_days$station_nodes |>
  dplyr::group_by(day) |>
  dplyr::summarise(n_hauls = n()) |>
  dplyr::group_by(n_hauls) |>
  dplyr::summarise(n = n())

vessel_days$total_days

total_duration <- vessel_days$total_days
sum(vessel_tsp$distance_nodes$distance)

par(mar = c(0,0,3,0))
for (iday in total_duration:1) {
  
  ## Plot all the vessel's station in lightgrey
  plot(st_geometry(vessel_dist), 
       col = "lightgrey", pch = 16, cex = 0.5,
       main = paste("Day", iday))
  
  ## Plot the station completed up to the iday-th day in black
  subset(vessel_dist,
         STATION %in% vessel_days$station_nodes$STATION[
           vessel_days$station_nodes$day %in% 1:iday
         ]) %>%
    st_geometry() %>% 
    plot(add = TRUE, pch = 16, col = "black")
  
  ## Plot the station completed on the iday-th day in blue
  subset(vessel_dist,
         STATION %in% vessel_days$station_nodes$STATION[
           vessel_days$station_nodes$day == iday
         ]) %>%
    st_geometry() %>% 
    plot(add = TRUE, pch = 16, col = "blue")
  
  ## Plot the line path of the vessel according to the station node order
  vessel_days$station_nodes %>% 
    subset(subset = day <= iday)  %>%
    st_as_sf(coords = c("x", "y"), crs = "EPSG:3338") %>%
    st_transform("EPSG:32606") %>% 
    st_coordinates() %>% 
    st_linestring() %>% 
    st_sfc(crs = "EPSG:32606") %>% 
    st_geometry() %>% 
    plot(add = TRUE)
}
