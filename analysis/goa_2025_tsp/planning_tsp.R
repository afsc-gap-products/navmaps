# Traveling salesman solution to survey time

library(TSP)
library(here)
library(dplyr)
library(sf)

# Setup parameters
max_daily_hr = 12 # Maximum hours worked in a day
processing_time_hr = 1.5 # Minimum processing time in hours
max_daily_stn = 6 # Maximum number of stations sampled in a day
transit_speed_kmh = 1.852*7.5 # Transit speed between stations in kilometers/hour
set_retrieve_hr = 0.5 # Time to set and retrieve the gear in hours
set_on_arrival = TRUE # Set on arrival at the station or set based on the minimum processing time?
vessel = 148


# Calculations with station allocation ----

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
#' vessel_dist <- system.file("extdata", "goa_station_allocation_2023.shp") |> 
#' sf::st_read() |>
#'   sf::st_transform(crs = "EPSG:32606") |>
#'   dplyr::filter(VESSEL == 148)
#' 
#' # Solve TSP for station order
#' tsp_out <- planning_solve_station_tsp(x = vessel_dist)
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
  function(x) {
    
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
    
    return(
      list(
        distance_nodes = vessel_dist, 
        tsp = vessel_tsp)
    )
    
  }



#' Estimate the number of days required to complete the survey
#'
#' This function calculates the number of days needed to complete a survey, given station distances, vessel transit speeds, and operational constraints (work hours, setting time, sample processing time, approach to setting).
#'
#' @param station_nodes A data frame or `sf` object containing survey stations with distances between consecutive stations; output from `planning_solve_station_tsp()`
#' @param max_daily_hr Maximum working hours per day.
#' @param processing_time_hr Minimum processing time at a station (in hours).
#' @param max_daily_stn Maximum number of stations sampled per day.
#' @param transit_speed_kmh Transit speed between stations (in kilometers per hour).
#' @param set_retrieve_hr Time required to set and retrieve gear (in hours).
#' @param set_on_arrival Logical; if `TRUE`, gear is set upon arrival at a 
#' station rather than after the minimum processing time.
#'
#' @return The estimated number of days required to complete the survey.
#'
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Load and transform survey stations
#' vessel_dist <- system.file("extdata", "goa_station_allocation_2023.shp") |> 
#' sf::st_read() |>
#'   sf::st_transform(crs = "EPSG:32606") |>
#'   dplyr::filter(VESSEL == 148)
#' 
#' # Solve TSP for station order
#' tsp_out <- planning_solve_station_tsp(x = vessel_dist)
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

planning_calc_survey_days <- 
  function(
    station_nodes,
    max_daily_hr, 
    processing_time_hr,
    max_daily_stn, 
    transit_speed_kmh,
    set_retrieve_hr,
    set_on_arrival = FALSE
  ) {
    
    station_nodes <- sf::st_drop_geometry(station_nodes)
    
    day <- 1
    hours_elapsed <- 0
    day_stations <- 0
    ii <- 1
    
    while(ii < nrow(station_nodes)) {
      
      
      if(!set_on_arrival) {
        transit_hours <- max(
          c(
            (processing_time_hr - set_retrieve_hr) + station_nodes$distance[ii+1] / transit_speed_kmh,
            processing_time_hr
          )
        )
      } else {
        transit_hours <- station_nodes$distance[ii+1] / transit_speed_kmh
      }
      
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


# Ocean Explorer

# 2025 design
vessel_dist <- sf::st_read(dsn = here::here("assets", "data", "allocation", "goa_2025_station_allocation_520_aea.gpkg")) |>
  sf::st_transform(crs = "EPSG:32606") |> # UTM Zone 6 CRS
  dplyr::filter(VESSEL == vessel) # OEX

tsp_out <- planning_solve_station_tsp(x = vessel_dist)

planning_calc_survey_days(
  station_nodes = tsp_out$distance_nodes,
  max_daily_hr = max_daily_hr, 
  processing_time_hr = processing_time_hr,
  max_daily_stn = max_daily_stn, 
  transit_speed_kmh = transit_speed_kmh,
  set_retrieve_hr = set_retrieve_hr,
  set_on_arrival = set_on_arrival
)