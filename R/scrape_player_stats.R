rm(list = ls())

library(rvest)
library(dplyr)
library(purrr)
library(stringr)

scrape_data <- function(start_year, end_year) {
  base_url <- "https://www.basketball-reference.com/leagues/NBA_"
  years <- start_year:end_year
  
  raw_scrape_path <- file.path("data-raw/", "1977-2019_bball_reference_scrape.csv")
  
  if (file.exists(raw_scrape_path)) {
    read.csv(raw_scrape_path, 
             stringsAsFactors = FALSE)
  } else {
    done <- c()
    scrape_list <- list()
    for (year in years) {
      while (!year %in% done) {
        message("Scraping ", year)
        
        tryCatch({
          scrape_list[[length(scrape_list)+1]] <-
            read_html(paste0(base_url, year, "_per_game.html")) %>% 
            html_table() %>% 
            when(class(.) == "list" ~ (.) %>% as.data.frame(stringsAsFactors = FALSE),
                 ~ .) %>% 
            mutate(yr = year)
          
          done <- c(done, year)
        },
        error = function(cond) {
          message("\n\nProblem with ", paste0(base_url, year, "_per_game.html"), "\n")
          message(cond)
        })
        
      }
    }
    
    full_df <-
      bind_rows(scrape_list)
    
    write.csv(full_df,
              raw_scrape_path, 
              row.names = FALSE)
    
    full_df
  }
}

clean_scrape <- function(df) {
  df %>%
    # Get rid of header rows in middle of table
    filter(Rk != "Rk") %>% 
    mutate_at(vars(G:PS.G), as.numeric) %>% 
    group_by(yr, Player) %>% 
    # TODO: How to define minutes per game per season for players on different teams? Currently weighted if on different teams
    mutate(mpg_per_year = sum(MP * G)/sum(G)) %>% 
    ungroup() %>% 
    group_by(Player) %>% 
    # TODO: How to define overall minutes per game
    mutate(mpg_overall = sum(mpg_per_year)/n()) %>% 
    ungroup() %>% 
    distinct(yr, Player, .keep_all = TRUE)
}


scrape_data(1977, 2019) %>% 
  clean_scrape() %>% 
  write.csv("data/1977-2019_bball_reference_clean.csv",
            row.names = FALSE)
  
  
  

