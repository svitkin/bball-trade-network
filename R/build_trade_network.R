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

extract_transactions <- function(url) {
  transactions_df <-
    read_html(url) %>% 
    html_table(header = TRUE) %>% 
    `[[`(1) %>% 
    filter(str_detect(Notes, "trade") | 
             str_detect(Notes, "^signed.*free agent") |
             str_detect(Notes, "player became a free agent") |
             str_detect(Notes, "waived"),
           !str_detect(Notes, "error"),
           !str_detect(Notes, "voided"))
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
                   message("\n\n###PROBLEM###\n", url, "is a problem")
                   problem_urls <<- c(problem_urls, url)
                   return(data.frame())
                 })
    }
    
    while (length(problem_urls) != 0) {
      for (url in problem_urls) {
        message("\n\n###WAS A PROBLEM###\n", url)
        transaction_dfs[[length(transaction_dfs)+1]] <- 
          tryCatch({
            extracted_table <- extract_transactions(url)
            problem_urls <<- setdiff(problem_urls, url)
            extracted_table
          }, error = function(cond) {
            message(cond)
            problem_urls <<- c(problem_urls, url)
            data.frame()
          })
      }
    }
    
    transaction_df <- bind_rows(transaction_dfs)
    write.csv(transaction_df, filename)
    transaction_df
  }
}

clean_transactions <- function(transactions_df, .remove_future_picks = TRUE, .remove_not_exercised_picks = TRUE) {
  split_trade_rows <- function(df) {
    df %>% 
      mutate(Acquired = str_split(Acquired, "•")) %>% 
      unnest() %>% 
      mutate(Relinquished = str_split(Relinquished, "•")) %>% 
      unnest() %>% 
      mutate(Relinquished = ifelse(str_detect(Notes, "^signed.*free agent"),
                                   "free agency",
                                   Relinquished),
             Acquired = ifelse(str_detect(Notes, "waived") | str_detect(Notes, "player became a free agent"),
                               "free agency",
                               Acquired))
  }
  remove_future_picks <- function(df) {
    df %>% 
      filter(!(str_detect(Acquired, "pick") & str_detect(Acquired, "\\?")), 
             !(str_detect(Relinquished, "pick") & str_detect(Relinquished, "\\?")))
  }
  remove_not_exercised_picks <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "not exercised"), 
             !str_detect(Acquired, "voided"),
             !str_detect(Relinquished, "not exercised"),
             !str_detect(Relinquished, "voided"))
  }
  clean_pick_text <- function(df) {
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "pick") & 
                                 str_detect(Acquired, "\\(.+#.+\\)$"),
                               str_c(str_extract(Acquired, "\\d{4}"), 
                                     str_extract(str_replace(Acquired,
                                                             "\\(\\d{4} ", "("), 
                                                 "\\(#.+\\)$"),
                                     sep = " pick "),
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "pick") & 
                                     str_detect(Relinquished, "\\(.+#.+\\)$"),
                                   str_c(str_extract(Relinquished, "\\d{4}"), 
                                         str_extract(str_replace(Relinquished,
                                                                 "\\(\\d{4} ", "("), 
                                                     "\\(#.+\\)$"),
                                         sep = " pick "),
                                   Relinquished),
             
             pick_involved = str_detect(Acquired, "pick") | str_detect(Relinquished, "pick"),
             
             Acquired = str_replace_all(Acquired, "\\d{4}.* pick \\(#\\d+\\-", ""),
             Acquired = str_replace_all(Acquired, "\\)", ""),
             Relinquished = str_replace_all(Relinquished, "\\d{4}.* pick \\(#\\d+\\-", ""),
             Relinquished = str_replace_all(Relinquished, "\\)", ""))
  }
  clean_cash_text <- function(df) {
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "cash"), "cash", Acquired),
             Acquired = ifelse((str_detect(Acquired, "$") & str_detect(Acquired, "\\d+") & !str_detect(Acquired, "[A-Za-z]")) | str_detect(Acquired, "\\d+K$"),
                               "cash",
                               Acquired),
             
             Relinquished = ifelse(str_detect(Relinquished, "cash"), "cash", Relinquished),
             Relinquished = ifelse((str_detect(Relinquished, "$") & str_detect(Relinquished, "\\d+") & !str_detect(Relinquished, "[A-Za-z]") | str_detect(Relinquished, "\\d+K$")),
                                   "cash",
                                   Relinquished))
  }
  clean_exception_text <- function(df) {
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "exception") | str_detect(Acquired, "exemption"), 
                               "trade exception", Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "exception") | str_detect(Relinquished, "exemption"), 
                                   "trade exception", Relinquished))
  }
  clean_rights_to_text <- function(df) {
    df %>% 
      
      mutate(rights_involved = str_detect(Acquired, "rights to") | str_detect(Relinquished, "rights to"),
             Acquired = ifelse(str_detect(Acquired, "rights to"),
                               str_replace(Acquired, "rights to", ""),
                               Acquired),
             Acquired = str_trim(Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "rights to"),
                                   str_replace(Relinquished, "rights to", ""),
                                   Relinquished),
             Relinquished = str_trim(Relinquished))
  }
  make_transaction_key <- function(df) {
    df %>% 
      mutate(key = paste0(pmin(Acquired, Relinquished),
                          pmax(Acquired, Relinquished))) %>% 
      distinct(key, .keep_all = TRUE)
  }
  make_edge_label <- function(df) {
    df %>% 
      mutate(edge_label = case_when(str_detect(Notes, "trade") ~ sprintf("On %s, %s make %s for %s, and in exhchange give %s to them.",
                                                                         Date, Team, Notes, Acquired, Relinquished),
                                    str_detect(Notes, "^signed.*free agent") ~ sprintf("On %s, %s is %s by %s",
                                                                                     Date, Acquired, Notes, Team),
                                    str_detect(Notes, "waived") | str_detect(Notes, "player became a free agent") ~ sprintf("On %s, %s becomes a free agent from %s",
                                                                          Date, Relinquished, Team)))
  }
  find_teams_involved <- function(df) {
    df %>% 
      mutate(teams_involved = case_when(Acquired != "free agency" & 
                                          Relinquished != "free agency" ~ str_replace_all(edge_label,
                                                                                          ".*make.*trade with (.+) for.*",
                                                                                          str_c(Team, "\\1", sep = ", ")),
                                        Acquired == "free agency" | Relinquished == "free agency" ~ Team))
  }
  
  message("Cleaning transactions")
  transactions_df %>% 
    split_trade_rows() %>% 
    filter(Acquired != "" & Relinquished != "") %>% 
    when(.remove_future_picks ~ remove_future_picks(.),
         ~ .) %>% 
    when(.remove_not_exercised_picks ~ remove_not_exercised_picks(.),
         ~ .) %>% 
    distinct() %>% 
    mutate(Acquired = str_trim(Acquired),
           Relinquished = str_trim(Relinquished)) %>% 
    make_edge_label() %>% 
    clean_pick_text() %>% 
    clean_cash_text() %>% 
    clean_exception_text() %>% 
    clean_rights_to_text() %>% 
    find_teams_involved() %>% 
    make_transaction_key()
}

write_out_edgelist_df <- function(start, end) {
  filename <- file.path("data", paste0(start, "_", end, "_edgelist-df.csv"))
  load_or_cache_date_files(start, end) %>% 
    clean_transactions() %>% 
    select(key,
           date = Date,
           to = Acquired,
           from = Relinquished,
           action_team = Team,
           notes = Notes,
           edge_label,
           pick_involved,
           rights_involved,
           teams_involved) %>% 
    write.csv(filename, row.names = FALSE)
}

write_out_edgelist_df("2007-01-01", Sys.Date())
  
 


