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
    message("Loading trade files")
    read.csv(filename, stringsAsFactors = FALSE)
  } else {
    message("Pulling and caching trade files")
    transaction_dfs <- list()
    problem_urls <- c()
    
    urls <- 
      create_base_url(start, end) %>% 
      create_all_page_urls()
    
    for (url in urls) {
      message(url)
      transaction_dfs[[length(transaction_dfs)+1]] <- 
        tryCatch(extract_transactions(url),
                 error = function(cond) {
                   message(url, "is a problem")
                   problem_urls <- c(problem_urls, url)
                   return(NA)
                 })
    }
    
    while (length(problem_urls) != 0) {
      for (url in problem_urls) {
        transaction_dfs[[length(transaction_dfs)+1]] <- 
          tryCatch({
            extracted_table <- extract_transactions(url)
            problem_urls <- setdiff(problem_urls, url)
            return(extracted_table)
          }, error = function(cond) {
            message(cond)
            return(NA)
          })
      }
    }
    
    transaction_df <- do.call(rbind, transaction_dfs[which(!is.na(transaction_dfs))])
    write.csv(transaction_df, filename)
    transaction_df
  }
}

clean_transactions <- function(transactions_df, .keep_picks = FALSE) {
  message("Cleaning transactions")
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
    clean_distinct_df <-
      clean_df %>% 
      mutate(Acquired = str_trim(Acquired),
             Relinquished = str_trim(Relinquished)) %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "pick") & 
                                 str_detect(Acquired, "\\(.+\\)$"),
                               str_c(str_extract(Acquired, "\\d{4}"), 
                                     str_extract(Acquired, "\\(#.+\\)$"),
                                     sep = " pick "),
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "pick") & 
                                     str_detect(Relinquished, "\\(.+\\)$"),
                                   str_c(str_extract(Relinquished, "\\d{4}"), 
                                         str_extract(Relinquished, "\\(#.+\\)$"),
                                         sep = " pick "),
                                   Relinquished),
             
             pick_involved = str_detect(Acquired, "pick") | str_detect(Relinquished, "pick"),
             
             Acquired = ifelse(str_detect(Acquired, "cash"), "cash", Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "cash"), "cash", Relinquished),
             Acquired = ifelse((str_detect(Acquired, "$") & str_detect(Acquired, "\\d+") & !str_detect(Acquired, "[A-Za-z]")) | str_detect(Acquired, "\\d+K$"),
                               "cash",
                               Acquired),
             Relinquished = ifelse((str_detect(Relinquished, "$") & str_detect(Relinquished, "\\d+") & !str_detect(Relinquished, "[A-Za-z]") | str_detect(Relinquished, "\\d+K$")),
                               "cash",
                               Relinquished),
             
             Acquired = ifelse(str_detect(Acquired, "exception") | str_detect(Acquired, "exemption"), 
                               "trade exception", Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "exception") | str_detect(Relinquished, "exemption"), 
                                   "trade exception", Relinquished)
             ) %>% 
      
      mutate(key = paste0(pmin(Acquired, Relinquished),
                          pmax(Acquired, Relinquished))) %>% 
      distinct(key, .keep_all = TRUE)
    
    if (!.keep_picks) {
      clean_distinct_df %>% 
        filter(!(str_detect(Acquired, "pick") & str_detect(Acquired, "\\?")),
               !(str_detect(Relinquished, "pick") & str_detect(Relinquished, "\\?")))
    } else {
      clean_distinct_df
    }
    
  } else {
    clean_df
  }
}

write_out_edgelist_df <- function(start, end) {
  filename <- file.path("data", paste0(start, "_", end, "_edgelist-df.csv"))
  load_or_cache_date_files(start, end) %>% 
    clean_transactions() %>% 
    mutate(edge_label = sprintf("On %s, %s make %s for %s, and in exhchange give %s to them.",
                                Date, Team, Notes, Acquired, Relinquished)) %>% 
    select(date = Date,
           from = Acquired,
           to = Relinquished,
           action_team = Team,
           notes = Notes,
           edge_label,
           pick_involved) %>% 
    write.csv(filename, row.names = FALSE)
}

write_out_edgelist_df("2007-01-01", Sys.Date())
  
 


