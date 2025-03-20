#' Solve a traveling Salesman problem (TSP) for a survey station allocation
#'
#' This function optimizes the sampling order of stations by solving a Traveling Salesman Problem (TSP) using the nearest insertion method to minimize the distance traveled. Option to use a circuit (starting and ending at the same station) or path (visit each station once without repeats).
#'
#' @param x An `sf` object representing the survey stations.
#' @param hamilton Should a Hamiltonian path be calculated instead of a circuit?
#'
#' @return A list containing:
#' \item{distance_nodes}{An `sf` object of survey stations ordered based on 
#' the optimal TSP solution, with an added column for inter-station distances in kilometers.}
#' \item{tsp}{The TSP solution object.}
#' @importFrom sf st_distance st_transform
#' @importFrom TSP as.ATSP TSP solve_TSP
#' @importFrom dplyr filter
#' @author Zack Oyafuso
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
                tsp = tsp_solution,
                total_distance_km = cumsum(x$distance))
    )
    
  }