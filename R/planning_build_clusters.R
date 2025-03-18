#' Build station clusters based on minimum transit distance
#'
#' This function groups nodes (stations) into clusters based on their proximity, ensuring that each cluster contains `max_stations`. A cluster is created for each station, and clustering is done by iteratively selecting the nearest unvisited station.
#'
#' @param nodes An `sf` object containing station locations as POINT geometries.
#' @param max_stations Number of stations per cluster.
#'
#' @return A data frame where each row represents a starting station, with additional columns:
#' 	 - `station_rank`: The sampling order of station within its cluster.
#' 	 - `cumulative_distance_km`: Cumulative transit distance within the cluster.
#' 	 - `sequential_index`: The index of stations in the cluster.
#'
#' @import sf
#' @export

planning_build_clusters <- function(nodes, max_stations = 6) {
  
  nodes$index <- 1:nrow(nodes)
  
  station_order_list <- vector(
    mode = "list", 
    length = nrow(nodes)
  )
  
  for(ii in 1:nrow(nodes)) {
    
    visited <- ii
    remaining <- setdiff(nodes$index, ii)
    
    while(length(visited) < max_stations) {
      
      last_node <- visited[length(visited)]
      dists <- sf::st_distance(nodes[last_node, ], nodes[remaining, ])
      
      # Find nearest node
      next_node <- remaining[which.min(dists)]
      visited <- c(visited, next_node)
      remaining <- setdiff(remaining, next_node)
      
    }
    
    station_distance <- 
      c(0,
        as.numeric(
          sf::st_distance(
            nodes[visited[1:max_stations-1], ], 
            nodes[visited[2:max_stations], ], 
            by_element = TRUE)/1e3
        )
      )
    
    station_order <-  
      do.call(
        rbind, 
        replicate(
          max_stations, 
          nodes[ii, ], 
          simplify = FALSE)
      )
    
    station_order$station_rank <- 1:max_stations
    station_order$cumulative_distance_km <- cumsum(station_distance)
    station_order$sequential_index <- visited
    
    station_order_list[[ii]] <- station_order
    
  }
  
  output <- do.call(rbind, station_order_list)
  
  return(output)
}