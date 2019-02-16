# With animation package
# Go through each date and make network of just that date
# Make sure all node and edge labels are visible and not just as tooltips
# Take image of that with animation package
# put it all together with animation package
# cache it as video (TODO: change?)
# have shiny play the video (TODO: change?)

# ALSO!!!
# only allow choosing a player or a team (maybe disable each)
# for a player animation have them choose how <player chosen> gets connected to <other player> through series of animations organized by path like above
# for a team animation, just show how all trades team is involved in by date
rm(list = ls())
library(dplyr)
library(igraph)
library(animation)
library(visNetwork)

edgelist <- read.csv("data/2010-01-01_2019-02-02_edgelist-df.csv", stringsAsFactors = FALSE)
player <- "Isaiah Thomas"
other_player <- "Lance Stephenson"

make_subgraph <- function(g, players, order) {
  subgraph_ids <-
    ego(g, order = order, nodes = players) %>% 
    unlist()
  
  induced_subgraph(g, subgraph_ids)
}

make_visNodes <- function(g) {
  nodes_df <- as_data_frame(g, "vertices")
  nodes_df %>% 
    rename(id = name) %>% 
    mutate(title = id,
           title = str_wrap(title, width = 40),
           title = str_replace_all(title, "\n", "<br>"),
           label = title)
}

add_edge_labels <- function(edges_df) {
  edges_df %>% 
    # TODO: Currently hacky fix to get edge labels, make better
    left_join(edgelist %>% select(from, to, 
                                      title = edge_label, 
                                      pick_involved, 
                                      rights_involved),
              by = c("from", "to")) %>% 
    left_join(edgelist %>% select(to = from, from = to, 
                                      title2 = edge_label, 
                                      pick_involved2 = pick_involved, 
                                      rights_involved2 = rights_involved),
              by = c("from", "to")) %>% 
    # Consolidate redundant columns
    mutate(title = ifelse(is.na(title), title2, title),
           pick_involved = ifelse(is.na(pick_involved), pick_involved2, pick_involved),
           rights_involved = ifelse(is.na(rights_involved), rights_involved2, rights_involved)) %>% 
    select(-title2, -pick_involved2, -rights_involved2)
}
add_edge_options <- function(edges_df) {
  edges_df %>% 
    # Options on edges
    mutate(color = case_when(pick_involved & rights_involved ~ "#CC7E0E",
                             pick_involved ~ "#0ECC47",
                             rights_involved ~ "#997E57",
                             TRUE ~ "#6BFFC1")) %>% 
    mutate(width = 8) %>% 
    distinct() %>% 
    select(-pick_involved, -rights_involved)
}
format_edge_labels <- function(edges_df) {
  edges_df %>% 
    mutate(title = str_split(title, "\n"),
           title = map(title, function(s) str_wrap(s, 80)),
           title = map(title, function(s) paste(s, collapse = "\n")),
           title = str_replace_all(title, "\n", "<br>"))
}
make_visEdges <- function(g) {
  as_data_frame(g, "edges")
}
make_visTitle <- function(edges_df) {
  edges_df %>% 
    add_edge_labels() %>% 
    format_edge_labels() %>% 
    rename(label = title) %>% 
    # mutate(label = str_replace_all(title, "<br>", "\n")) %>% 
    pull(label) %>% 
    unique() %>% 
    paste(collapse = "<br>")
}


subg <-
  edgelist %>% 
  filter(from != "cash",
         to != "cash",
         from != "trade exception",
         to != "trade exception",
         from != "free agency",
         to != "free agency",
         from != "draft") %>% 
  select(from, to, date, edge_label) %>% 
  graph_from_data_frame() %>% 
  make_subgraph(player, 7)


set.seed(123)
for (i in 1:(length(path_interest)-1)) {
  vis_subgraph <- induced_subgraph(subg, path_interest[c(i, i+1)])
  nodes_df <- make_visNodes(vis_subgraph)
  edges_df <- make_visEdges(vis_subgraph)
  
  visNetwork(nodes_df, edges_df, main = make_visTitle(edges_df)) %>% 
    visHierarchicalLayout() %>% 
    print()
}

