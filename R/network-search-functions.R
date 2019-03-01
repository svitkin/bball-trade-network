# rm(list = ls())
library(igraph)
library(dplyr)
library(purrr)
library(stringr)
library(collections)

date_ego <- function(g, gdf, player1, numSteps) {
  queue <- Deque$new()
  queue$push(player1)
  final_nodes <- list()
  freeAgencyPassed <- FALSE
  
  while (queue$size() > 0) {
    path <- queue$popleft()
    node <- path[[length(path)]]
    
    nbhd <- neighbors(g, node, "all")$name  
  
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
    min_date <- min(last_df$date)
    
    if (length(path) <= (numSteps + 1)) {
      final_nodes[[node]][[length(final_nodes[[node]]) + 1]] <- path
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

