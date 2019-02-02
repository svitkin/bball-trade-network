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
           !str_detect(Notes, "voided"),
           !str_detect(Notes, "rescinded")) %>% 
    mutate(page_url = url)
}


load_or_cache_date_files <- function(start, end) {
  filename <- file.path("data-raw", paste0(start, "_", end, "_transactions.csv"))
  if (file.exists(filename)) {
    message("Loading trade files")
    read.csv(filename, stringsAsFactors = FALSE)
  } else {
    message("Pulling and caching trade files")
    transaction_dfs <- list()
    done <- c()
    urls <- 
      create_base_url(start, end) %>% 
      create_all_page_urls()
    
    for (url in urls) {
      while (!url %in% done) {
          tryCatch({
            transaction_dfs[[length(transaction_dfs)+1]] <-
              extract_transactions(url)
            done <- c(done, url)
          },
          error = function(cond) {
            message("\n\n###PROBLEM###\n", url)
            message(cond)
          })  
      }
    }
    
    transaction_df <- bind_rows(transaction_dfs)
    write.csv(transaction_df, filename, row.names = FALSE)
    transaction_df
  }
}

clean_transactions <- function(transactions_df, 
                               .remove_future_picks = TRUE, 
                               .remove_not_exercised_picks = TRUE) {
  
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
      mutate(pick_involved = str_detect(Acquired, "pick") | str_detect(Relinquished, "pick"),
             Acquired = ifelse(str_detect(Acquired, "pick") & 
                                 str_detect(Acquired, "\\(.*#.*\\)$"),
                               str_replace_all(Acquired, ".*\\(.*#.*\\-(.*)\\)$", "\\1"),
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "pick") & 
                                 str_detect(Relinquished, "\\(.*#.*\\)$"),
                               str_replace_all(Relinquished, ".*\\(.*#.*\\-(.*)\\)$", "\\1"),
                               Relinquished))
  }
  clean_cash_text <- function(df) {
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "cash"), "cash", Acquired),
             Acquired = ifelse((str_detect(Acquired, "$") & str_detect(Acquired, "\\d+")) &
                                 (!str_detect(Acquired, "[A-Za-z]") | 
                                    str_detect(Acquired, "\\d+K$") |
                                    str_detect(Acquired, "\\d+M")),
                               "cash",
                               Acquired),
             
             Relinquished = ifelse(str_detect(Relinquished, "cash"), "cash", Relinquished),
             Relinquished = ifelse((str_detect(Relinquished, "$") & str_detect(Relinquished, "\\d+")) & 
                                     (!str_detect(Relinquished, "[A-Za-z]") | 
                                        str_detect(Relinquished, "\\d+K$") | 
                                        str_detect(Relinquished, "\\d+M")),
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
      mutate(key = ifelse(Acquired == "free agency" | Relinquished == "free agency",
                          paste0(pmin(Acquired, Relinquished),
                                 pmax(Acquired, Relinquished),
                                 Notes),
                          paste0(pmin(Acquired, Relinquished),
                                 pmax(Acquired, Relinquished)))) %>% 
      distinct(key, .keep_all = TRUE)
  }
  make_edge_label <- function(df) {
    df %>% 
      mutate(edge_label = case_when(str_detect(Notes, "trade") ~ sprintf("In a %s, on %s, the %s receive %s in exchange for %s",
                                                                         str_replace(Notes, "with", "with the"), 
                                                                         Date, Team, Acquired, Relinquished),
                                    str_detect(Notes, "^signed.*free agent") ~ sprintf("On %s, %s is %s by %s",
                                                                                     Date, Acquired, 
                                                                                     str_replace(Notes, "signed", "signed as"), 
                                                                                     Team),
                                    str_detect(Notes, "waived") | str_detect(Notes, "player became a free agent") ~ sprintf("On %s, %s becomes a free agent from %s",
                                                                          Date, Relinquished, Team)))
  }
  find_teams_involved <- function(df) {
    df %>% 
      mutate(
        # Fix typos
        Notes = str_replace(Notes, "wtih", "with"),
        Notes = str_replace(Notes, "Timberwoves", "Timberwolves"),
        Team = str_replace(Team, "Timberwoves", "Timberwolves"),
        # Extract names
        teams_involved = 
          case_when(Acquired != "free agency" & 
                      Relinquished != "free agency" ~ str_replace_all(Notes,
                                                                      ".*with (.+)",
                                                                      str_c(Team, "\\1", sep = ", ")),
                    Acquired == "free agency" | Relinquished == "free agency" ~ Team))
  }
  misc_cleanup <- function(df) {
    # Random one offs that need to be cleaned
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "Cabarrot") | str_detect(Acquired, "2016 #24"),
                               "Timothe Luwawu / Timothe Luwawu-Cabarrot",
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "Cabarrot") | str_detect(Relinquished, "2016 #24"),
                               "Timothe Luwawu / Timothe Luwawu-Cabarrot",
                               Relinquished))
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
    misc_cleanup() %>% 
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

write_out_edgelist_df("2010-01-01", "2019-02-01")
  
 


