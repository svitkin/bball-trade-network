rm(list = ls())
library(igraph)
library(dplyr)
library(purrr)
library(stringr)
library(collections)

date_ego <- function(g, gdf, player1, numSteps) {
  all_nodes <- V(g)$name
  visited <- as.list(rep(FALSE, length(all_nodes)))
  names(visited) <- all_nodes
  
  queue <- Deque$new()
  queue$push(player1)
  visited[[player1]] <- TRUE
  final_nodes <- c()
  
  while (queue$size() > 0) {
    path <- queue$popleft()
    node <- path[length(path)]
    nbhd <- neighbors(g, node, "all")$name
    if (length(path) == numSteps) {
      final_nodes <- c(final_nodes, path)
    } else {
      for (nbhr in nbhd) {
        if (!(length(path) + 1) %in% visited[[nbhr]] & !nbhr %in% path) {
          if (length(path) >= 2) {
            last_df <- 
              gdf %>% 
              filter((from == node & to == path[length(path)-1]) |
                       (to == node & from == path[length(path)-1])) 
          } else {
            last_df <-
              gdf %>% 
              filter(from == node | to == node)
          }
          
          min_date <- min(last_df$date)
          teams_keep <- 
            unique(c(last_df$from_team, last_df$to_team))
          
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
          
          # browser()
          if (nrow(next_df) > 0) {
            dates <- unique(next_df$date)
            if (any(dates >= min_date)) {
              new_path <- c(path, nbhr)
              message("New path is ", new_path)
              queue$push(new_path)
              visited[[nbhr]] <- c(visited[[nbhr]], length(new_path)) 
            } else {
              visited[[nbhr]] <- c(visited[[nbhr]], length(path) + 1)
            } 
            
          } else {
            visited[[nbhr]] <- c(visited[[nbhr]], length(path) + 1)
          } 
        }
      }
    }
  }
  return(final_nodes)
}
date_search <- function(g, gdf, player1, player2) {
  all_nodes <- V(g)$name
  visited <- as.list(rep(c(0), length(all_nodes)))
  names(visited) <- all_nodes
  
  queue <- Deque$new()
  queue$push(player1)
  visited[[player1]] <- c(1)
  
  while (queue$size() > 0) {
    
    path <- queue$popleft()
    node <- path[length(path)]
    nbhd <- neighbors(g, node, "all")$name
    
    for (nbhr in nbhd) {
      if (node == player2) {
        message("Adding path!")
        # final_results$push(path)
        return(path)
      }
      # if (node == "Eric Williams") browser()
      if (!(length(path) + 1) %in% visited[[nbhr]] & !nbhr %in% path) {
        
        if (length(path) >= 2) {
          last_df <- 
            gdf %>% 
            filter((from == node & to == path[length(path)-1]) |
                     (to == node & from == path[length(path)-1])) 
        } else {
          last_df <-
            gdf %>% 
            filter(from == node | to == node)
        }
        
        min_date <- min(last_df$date)
        teams_keep <- 
          unique(c(last_df$from_team, last_df$to_team))
        
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
        
        # browser()
        if (nrow(next_df) > 0) {
          dates <- unique(next_df$date)
          if (any(dates >= min_date)) {
            new_path <- c(path, nbhr)
            message("New path is ", new_path)
            queue$push(new_path)
            visited[[nbhr]] <- c(visited[[nbhr]], length(new_path)) 
          } else {
            visited[[nbhr]] <- c(visited[[nbhr]], length(path) + 1)
          } 
          
        } else {
          visited[[nbhr]] <- c(visited[[nbhr]], length(path) + 1)
        } 
      }
      
    }
    
  }
  return(final_results$as_list())
}

gdf <- 
  read.csv("data/1976-11-16_2019-02-24_edgelist-df.csv",
           stringsAsFactors = FALSE) %>% 
  select(from, to, date, edge_label, pick_involved, rights_involved, to_team = action_team) %>%
  filter(!from %in% c("free agency", "draft", "cash", "trade exception"),
         !to %in% c("free agency", "draft", "cash", "trade exception")) %>% 
  mutate(from_team = map(edge_label, function(x) paste(str_replace_all(str_replace_all(unlist(str_extract_all(x, "\n.* acquire")), "acquire", ""), "\n", ""), collapse = "")),
         from_team = str_trim(str_replace_all(from_team, to_team, "")))

g <- 
  graph_from_data_frame(gdf)

player1 <- "Vince Carter"
player2 <- "Luke Ridnour"
numSteps <- 6
mode <- "all"

g_sub <- 
  ego(g, order = numSteps, "Vince Carter", "all") %>% 
  unlist() %>% 
  names() %>% 
  induced_subgraph(g, .)

results <- date_search(g_sub, gdf, player1, player2)

