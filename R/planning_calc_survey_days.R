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
#' @param set_on_arrival Logical; if `TRUE`, gear is set upon arrival at a station rather than after the minimum processing time.
#'
#' @return The estimated number of days required to complete the survey.
#'
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' # Load and transform survey stations
#' vessel_dist <- system.file("extdata", "goa_station_allocation_520.shp", package = "navmaps") |> 
#' sf::st_read() |>
#'   sf::st_transform(crs = "EPSG:32606") |> # UTM zone 2
#'   dplyr::filter(VESSEL == 148) # Ocean Explorer
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
    
    station_nodes = station_cluster
    max_daily_hr = max_daily_hr
    processing_time_hr = processing_time_hr
    max_daily_stn = max_daily_stn
    transit_speed_kmh = transit_speed_kmh
    set_retrieve_hr = set_retrieve_hr
    set_on_arrival = TRUE
    
    station_nodes <- sf::st_drop_geometry(station_nodes)
    station_nodes$day <- NA
    station_nodes$hours_elapsed <- NA
    
    day <- 1
    hours_elapsed <- 0
    day_stations <- 0
    ii <- 1
    
    while(ii < nrow(station_nodes)) {
        
      # Case where you either set on arrival or processing time is less than the transit time
      transit_hours <- set_retrieve_hr + station_nodes$distance[ii+1] / transit_speed_kmh
      
      if(transit_hours < processing_time_hr & !set_on_arrival) { 
        
        transit_hours <- processing_time_hr 
      }
      
      day_stations <- day_stations + 1
      
      if(transit_hours > max_daily_hr) {
        
        day <- day + 1
        hours_elapsed <- 0
        day_stations <- 0 
        ii <- ii + 1
        
      } else if(
        hours_elapsed + transit_hours > max_daily_hr & transit_hours < max_daily_hr || # Don't set if you exceed max_daily_hr
                max_daily_stn < day_stations # Don't set if you've exceeded the maximum number of stations
        ) {
        
        day <- day + 1
        hours_elapsed <- 0
        day_stations <- 0 
        
      } else {
        
        hours_elapsed <- hours_elapsed + transit_hours
        station_nodes$day[ii] <- day
        station_nodes$hours_elapsed[ii] <- hours_elapsed
        
        ii <- ii + 1
        
      } 
      
    }
    
    station_nodes$day[nrow(station_nodes)] <- day
    
    return(
      list(total_days = day,
           station_nodes = station_nodes
           )
      )
    
  }