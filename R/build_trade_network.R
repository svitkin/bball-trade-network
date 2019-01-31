rm(list=ls())

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)


create_base_url <- function(start, end) {
  sprintf("http://prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=%s&EndDate=%s&PlayerMovementChkBx=yes&Submit=Search", start, end)
}
create_all_page_urls <- function(base_url) {
  
  pages <- 
    read_html(base_url) %>% 
    html_nodes(".bodyCopy") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    as.numeric() %>% 
    na.omit()
  
  c(base_url,
    paste0(base_url, "&start=", (pages-1) * 25))
  
}

extract_transactions <- function(url, .trade = TRUE) {
  transactions_df <-
    read_html(url) %>% 
    html_table(header = TRUE) %>% 
    `[[`(1)
  
  if (.trade) {
    transactions_df %>% 
      filter(str_detect(Notes, "trade"))
  } else {
    transactions_df
  }
}


load_or_cache_date_files <- function(start, end) {
  filename <- file.path("data-raw", paste0(start, "_", end, "_transactions.csv"))
  if (file.exists(filename)) {
    read.csv(filename, stringsAsFactors = FALSE)
  } else {
    transactions_df <- 
      create_base_url(start, end) %>% 
      create_all_page_urls() %>% 
      map_df(extract_transactions)
    
    write.csv(transactions_df, filename)
    transactions_df
  }
}

clean_transactions <- function(transactions_df) {
  clean_df <-
    transactions_df %>% 
    mutate(Acquired = str_split(Acquired, "•")) %>% 
    unnest() %>% 
    mutate(Relinquished = str_split(Relinquished, "•")) %>% 
    unnest() %>% 
    # TODO: make this trade dependent or getting overly complex
    filter(Acquired != "" & Relinquished != "") %>% 
    distinct()
  
  if (nrow(clean_df) > 0) {
    clean_df %>% 
      mutate(Acquired = str_trim(Acquired),
             Relinquished = str_trim(Relinquished)) %>% 
      mutate(key = paste0(pmin(Acquired, Relinquished),
                          pmax(Acquired, Relinquished))) %>% 
      distinct(key, .keep_all = TRUE)
  } else {
    clean_df
  }
}

write_out_edgelist_df <- function(start, end) {
  filename <- file.path("data", paste0(start, "_", end, "_edgelist-df.csv"))
  load_or_cache_date_files(start, end) %>% 
    clean_transactions() %>% 
    select(date = Date,
           from = Acquired,
           to = Relinquished,
           action_team = Team,
           notes = Notes) %>% 
    write.csv(filename, row.names = FALSE)
}

write_out_edgelist_df("2018-01-01", "2019-01-31")
  
 


