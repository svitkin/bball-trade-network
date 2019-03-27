rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(igraph)

stats_data <- 
  read.csv("data/1977-2019_bball_reference_clean.csv",
           stringsAsFactors = FALSE) %>% 
  mutate(Player = str_replace_all(Player, "\\*", ""),
         Pos = case_when(
           str_detect(Pos, "G") ~ "G",
           str_detect(Pos, "F") ~ "F",
           TRUE ~ Pos
         ))

network_data <- 
  read.csv(file.path("data", "1976-11-16_2019-02-24_edgelist-df.csv"),
           stringsAsFactors = FALSE) %>% 
  select(from, to, date, edge_label, pick_involved, rights_involved, from_team, to_team) %>% 
  filter(!from %in% c("free agency", "draft", "trade exception", "cash"),
         !to %in% c("free agency", "draft", "trade exception", "cash"))

trade_network <-
  graph_from_data_frame(network_data)

degree_df <-
  degree(trade_network, mode = "all") %>% 
  data.frame(Player = names(.),
             degree = .,
             stringsAsFactors = FALSE) %>% 
  mutate(Player = str_replace_all(Player, "\\(.*\\)", ""),
         name1 = str_split(Player, " / "),
         name2 = map(name1, function(x) x[2]),
         name1 = map(name1, function(x) x[1]),
         name2 = unlist(name2),
         name1 = unlist(name1)) %>% 
  # 12 players are marked as duplicates with () and are removed
  group_by(Player) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 1) %>% 
  select(-n)

degree_stats_df <-
  bind_rows(degree_df %>% 
              left_join(stats_data %>% rename(name1 = Player),
                        by = "name1") %>% 
              filter(!is.na(G)),
            degree_df %>% 
              left_join(stats_data %>% rename(name2 = Player),
                        by = "name2") %>% 
              filter(!is.na(G)))

mpg_degree_pos_df <-
  degree_stats_df %>% 
  group_by(Player) %>% 
  # Get first year of data for position
  arrange(yr) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Player, Pos, mpg_overall, degree) %>% 
  filter(mpg_overall > 0) %>% 
  mutate(deg_mpg = degree/mpg_overall) %>% 
  group_by(Pos) %>% 
  arrange(desc(deg_mpg)) %>% 
  slice(1:5)



length(setdiff(degree_df$Player, degree_stats_df$Player))/nrow(degree_df)

