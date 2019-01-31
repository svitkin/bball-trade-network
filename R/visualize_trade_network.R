rm(list = ls())

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(igraph)
library(visNetwork)

edgelist_df <- read.csv("data/2018-01-01_2019-01-31_edgelist-df.csv", stringsAsFactors = FALSE)

visData <- 
  graph_from_data_frame(edgelist_df %>% select(from, to), directed = FALSE) %>% 
  visIgraph()
