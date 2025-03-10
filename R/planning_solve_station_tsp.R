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
    
    vessel_dist$node <- as.numeric(attr(vessel_tsp, "names"))
    vessel_dist <- vessel_dist[as.numeric(attr(vessel_tsp, "names")), ]
    vessel_dist$order <- 1:nrow(vessel_dist)
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