library(igraph)
library(dplyr)
library(purrr)
library(stringr)
library(collections)

date_ego <- function(g, gdf, player1, numSteps) {
  
  # Queue to store paths from player1
  queue <- Deque$new()
  # Initialize by adding player1
  queue$push(player1)
  # Initialize container for final paths
  final_nodes <- list()
  
  # While there are still paths to examine
  while (queue$size() > 0) {
    
    # Pop out path and last node in path
    path <- queue$popleft()
    node <- path[[length(path)]]
    
    # Find that last nodes neighbors
    nbhd <- neighbors(g, node, "all")$name  
  
    # To track dates and teams, find the last edge in path in edgelist df
    if (length(path) >= 2) {
      last_df <- 
        gdf %>% 
        filter((from == node & to == path[[length(path)-1]]) |
                 (to == node & from == path[[length(path)-1]])) 
    } else {
      last_df <-
        gdf %>% 
        filter(from == node | to == node)
    }
    
    # The earliest date of a trade between the last two players in path
    min_date <- min(last_df$date)
    
    # If the path length limit has not been reached
    if (length(path) <= (numSteps + 1)) {
      # Store path for the node being searched on
      final_nodes[[node]][[length(final_nodes[[node]]) + 1]] <- path
      
      # For each neighbor
      # Make sure you are not traversing a node already passed in this path
      # Check that this examined connection between the node and the neighbor 
      # Happens later than the min date and with the appropriate team matched to each player
      for (nbhr in nbhd) {
        if (!nbhr %in% path) {
          
          if (length(path) >= 2) {
            if (node %in% last_df$from) {
              next_df <-
                gdf %>% 
                filter((from_team %in% last_df$from_team & to == node & from == nbhr) |
                         (to_team %in% last_df$from_team & from == node & to == nbhr))
            } else {
              next_df <- data.frame()
            }
            
            if (node %in% last_df$to) {
              next_df <-
                gdf %>% 
                filter((from_team %in% last_df$to_team & to == node & from == nbhr) |
                         (to_team %in% last_df$to_team & from == node & to == nbhr)) %>% 
                bind_rows(next_df)
            }
          } else {
           next_df <-
             gdf %>% 
             filter((from == node & to == nbhr) |
                      (to == node & from == nbhr))
          }
          
          
          # browser()
          # If all the date and team checks pass then add the neighbor to the path
          # And push the new path to the queue to examine later
          if (nrow(next_df) > 0) {
            dates <- unique(next_df$date)
            if (any(dates >= min_date)) {
              new_path <- c(path, nbhr)
              # message("New path is ", new_path)
              queue$push(new_path)
            } 
          }
          
        }
      }
    }
  }
  return(final_nodes)
}

