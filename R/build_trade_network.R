rm(list=ls())

library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(assertr)


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
                               .remove_not_exercised_picks = TRUE,
                               .remove_modification_picks = TRUE) {
  
  filter_for_relevant_rows <- function(df) {
    df %>% 
      filter(str_detect(Notes, "trade") | 
               str_detect(Notes, "^signed.*free agent") |
               str_detect(Notes, "player became.*free agent") |
               str_detect(Notes, "waived") |
               str_detect(Notes, "claimed off waivers") |
               str_detect(Notes, "contract expired") |
               str_detect(Notes, "\\d{4} NBA draft.*round pick.*") |
               str_detect(Notes, "^first round pick") |
               str_detect(Notes, "^second round pick"),
             !str_detect(Notes, "error"),
             !str_detect(Notes, "voided"),
             !str_detect(Notes, "rescinded"))
  }
  add_unplaced_bullets <- function(df) {
    # For cases where bullets have been found to not have been properly placed
    df %>% 
      mutate(Acquired = str_replace(Acquired, "1996 first round pick \\(#11-Todd Fuller\\) 1998 first round pick \\(#5-Vince Carter\\)",
                                    "1996 first round pick (#11-Todd Fuller)•1998 first round pick (#5-Vince Carter)"),
             Relinquished = str_replace(Relinquished, "1996 first round pick \\(#11-Todd Fuller\\) 1998 first round pick \\(#5-Vince Carter\\)",
                                        "1996 first round pick (#11-Todd Fuller)•1998 first round pick (#5-Vince Carter)"))
      
  }
  split_trade_rows <- function(df) {
    df %>% 
      mutate(Acquired = str_split(Acquired, "•")) %>% 
      unnest() %>% 
      mutate(Relinquished = str_split(Relinquished, "•")) %>% 
      unnest() %>% 
      mutate(Relinquished = ifelse(str_detect(Notes, "^signed.*free agent") | 
                                     str_detect(Notes, "claimed off waivers"),
                                   "free agency",
                                   Relinquished),
             Relinquished = ifelse(str_detect(Notes, "\\d{4} NBA draft.*round pick.*") |
                                     str_detect(Notes, "^first round pick") |
                                     str_detect(Notes, "^second round pick"),
                                   "draft",
                                   Relinquished),
             Acquired = ifelse(str_detect(Notes, "waived") | 
                                 str_detect(Notes, "contract expired") | 
                                 str_detect(Notes, "player became.*free agent"),
                               "free agency",
                               Acquired))
  }
  remove_future_picks <- function(df) {
    df %>% 
      filter(!(str_detect(Acquired, "pick") & str_detect(Acquired, "\\?")), 
             !(str_detect(Relinquished, "pick") & str_detect(Relinquished, "\\?")),
             !str_detect(Acquired, "considerations"),
             !str_detect(Relinquished, "considerations"))
  }
  remove_non_agreements <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "agree.* not to"),
             !str_detect(Acquired, "agree.* to not"),
             !str_detect(Acquired, "agree.* to delay"),
             !str_detect(Acquired, "agree.* to release"),
             !str_detect(Relinquished, "agree.* not to"),
             !str_detect(Relinquished, "agree.* to not"),
             !str_detect(Relinquished, "agree.* to delay"),
             !str_detect(Relinquished, "agree.* to release"))
  }
  remove_not_exercised_picks <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "not exercised"), 
             !str_detect(Acquired, "voided"),
             !str_detect(Relinquished, "not exercised"),
             !str_detect(Relinquished, "voided"))
  }
  remove_modification_of_picks <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "protection"),
             !str_detect(Relinquished, "protection"))
    
  }
  remove_right_to <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, ".+right to"),
             !str_detect(Relinquished, ".+right to"))
  }
  remove_agreed_to_waive <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "agreed to waive"),
             !str_detect(Relinquished, "agreed to waive"),
             !str_detect(Acquired, "waived rights to"),
             !str_detect(Relinquished, "waived rights to"))
  }
  remove_was_removed <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "was removed"),
             !str_detect(Relinquished, "was removed"))
  }
  remove_unknown_signings <- function(df) {
    df %>% 
      filter(!str_detect(Acquired, "who had recently signed an offer sheet"),
             !str_detect(Relinquished, "who had recently signed an offer sheet"))
  }
  remove_unknown_picks <- function(df) {
    df %>% 
      filter(Acquired != "1998 first round pick",
             Relinquished != "1998 first round pick")
  }
  remove_cycles <- function(df) {
    # Gets rid of uninformative cash-cash and free agency-free agency relationships
    # as well as erroneous Jeremy Lin cycle
    df %>% 
      filter(Acquired != Relinquished)
  }
  clean_misc_pick_text <- function(df) {
    df %>% 
      mutate(Acquired = ifelse(Acquired == "1984 second round pick (#41-Tom Sluby) (1983 second round pick per NBA Register and Knicks media guide)",
                               "1984 second round pick (#41-Tom Sluby)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "1984 second round pick (#41-Tom Sluby) (1983 second round pick per NBA Register and Knicks media guide)",
                                   "1984 second round pick (#41-Tom Sluby)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "2000 second round pick (#58-Pete Mickeal) (not per Lakers media guide)",
                               "2000 second round pick (#58-Pete Mickeal)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "2000 second round pick (#58-Pete Mickeal) (not per Lakers media guide)",
                                   "2000 second round pick (#58-Pete Mickeal)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "2000 second round pick (#37-Eddie House (Ernest Brown per Nuggets media guide))",
                               "2000 second round pick (#37-Eddie House)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "2000 second round pick (#37-Eddie House (Ernest Brown per Nuggets media guide))",
                                   "2000 second round pick (#37-Eddie House)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "1998 first round pick (lottery protected) (#15-Matt Harpring)",
                               "1998 first round pick (#15-Matt Harpring)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "1998 first round pick (lottery protected) (#15-Matt Harpring)",
                                   "1998 first round pick (#15-Matt Harpring)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "third round pick (1982 first round pick per Lakers media guide) (1982 #54-Willie D. Jones)",
                               "1982 first round pick (#54-Willie D. Jones)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "third round pick (1982 first round pick per Lakers media guide) (1982 #54-Willie D. Jones)",
                                   "1982 first round pick (#54-Willie D. Jones)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "1994 first round pick (#19-Tony Dumas) (1993 pick per NBA Register, Mavericks media guide)",
                               "1994 first round pick (#19-Tony Dumas)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "1994 first round pick (#19-Tony Dumas) (1993 pick per NBA Register, Mavericks media guide)",
                                   "1994 first round pick (#19-Tony Dumas)",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(Acquired == "1997 conditional first round pick (1996 per Bucks media guide) (#20-Paul Grant)",
                               "1997 first round pick (#20-Paul Grant)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "1997 conditional first round pick (1996 per Bucks media guide) (#20-Paul Grant)",
                                   "1997 first round pick (#20-Paul Grant)",
                                   Relinquished)) %>% 
      mutate(Acquired = ifelse(Acquired == "1997 first round pick (1996 per Bucks media guide) (#20-Paul Grant)",
                               "1997 first round pick (#20-Paul Grant)",
                               Acquired),
             Relinquished = ifelse(Relinquished == "1997 first round pick (1996 per Bucks media guide) (#20-Paul Grant)",
                                   "1997 first round pick (#20-Paul Grant)",
                                   Relinquished))
    
    
    
  }
  clean_agreed_to_select <- function(df) {
    df %>% 
      mutate(Acquired = str_replace(Acquired, ".+agree.* to select (.*) from.*", "\\1"),
             Acquired = str_replace(Acquired, ".+agree.* to select (.*) in.*", "\\1"),
             Acquired = str_replace(Acquired, ".+agree.* to select (.*) \\(.*", "\\1"),
             Relinquished = str_replace(Relinquished, ".+agree.* to select (.*) from.*", "\\1"),
             Relinquished = str_replace(Relinquished, ".+agree.* to select (.*) in.*", "\\1"),
             Relinquished = str_replace(Relinquished, ".+agree.* to select (.*) \\(.*", "\\1"))
  }
  clean_player_to_be_named <- function(df) {
    df %>% 
      mutate(Acquired = str_replace_all(Acquired,
                                        "player to be named later \\((.*) on .*\\)",
                                        "\\1"),
             Relinquished = str_replace_all(Relinquished,
                                            "player to be named later \\((.*) on .*\\)",
                                            "\\1"))
  }
  clean_per_media_guide <- function(df) {
    df %>% 
      mutate(Acquired = str_replace(Acquired, 
                                    "\\(.*per.*media.*\\)",
                                    ""),
             Acquired = str_replace_all(Acquired, " {2,}", " "),
             Relinquished = str_replace(Relinquished, 
                                        "\\(.*per.*media.*\\)",
                                        ""),
             Relinquished = str_replace_all(Relinquished, " {2,}", " "))
  }
  clean_pick_text <- function(df) {
    df %>% 
      mutate(pick_involved = 
               str_detect(Acquired, "pick") | 
               str_detect(Relinquished, "pick") |
               str_detect(Notes, "\\d{4} NBA draft.*round pick.*") |
               str_detect(Notes, "^first round pick") |
               str_detect(Notes, "^second round pick"),
             Acquired = ifelse(str_detect(Acquired, "pick") & 
                                 str_detect(Acquired, "\\(.*#.*\\)$"),
                               str_replace_all(Acquired, ".*\\(.*#.*\\-(.*)\\)$", "\\1"),
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "pick") & 
                                 str_detect(Relinquished, "\\(.*#.*\\)$"),
                               str_replace_all(Relinquished, ".*\\(.*#.*\\-(.*)\\)$", "\\1"),
                               Relinquished))
  }
  clean_extraneous_parens <- function(df) {
    df %>% 
      mutate(Acquired = str_replace(Acquired, "\\) \\(\\d.*", ""),
             Acquired = str_replace(Acquired, "\\) \\(not.*", ""),
             Relinquished = str_replace(Relinquished, "\\) \\(\\d.*", ""),
             Relinquished = str_replace(Relinquished, "\\) \\(not.*", ""))
  }
  clean_restricted_fa_text <- function(df) {
    df %>% 
      mutate(Acquired = str_replace(Acquired, "restricted free agent ", ""),
             Relinquished = str_replace(Relinquished, "restricted free agent ", ""))
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
                               str_replace_all(Acquired, "rights to", ""),
                               Acquired),
             Acquired = str_trim(Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "rights to"),
                                   str_replace_all(Relinquished, "rights to", ""),
                                   Relinquished),
             Relinquished = str_trim(Relinquished))
  }
  make_transaction_key <- function(df) {
    df %>% 
      mutate(key = paste0(pmin(Acquired, Relinquished),
                          pmax(Acquired, Relinquished),
                          Date)) %>% 
      distinct(key, .keep_all = TRUE)
  }
  adjust_label_for_relinquished_team <- function(df) {
    
    # TODO: Fix weird edge cases
    # df %>% 
    #   left_join(df %>% 
    #               filter(str_detect(Notes, "\\d.*team trade") & 
    #                        !str_detect(Relinquished, "cash") &
    #                        !str_detect(Acquired, "cash")) %>% 
    #               select(Date, 
    #                      relinq_team = Team,
    #                      Relinquished = Acquired), 
    #             by = c("Date", "Relinquished")) %>% 
    #   distinct() %>% 
    #   group_by(Date, Acquired, Relinquished, relinq_team) %>% 
    #   mutate(n = n()) %>% 
    #   ungroup() %>% 
    #   filter(n > 1)
    df %>% 
      left_join(df %>% 
                  filter(str_detect(Notes, "\\d.*team trade") & 
                           !str_detect(Relinquished, "cash") &
                           !str_detect(Acquired, "cash")) %>% 
                  select(Date, 
                         relinq_team = Team,
                         Relinquished = Acquired), 
                by = c("Date", "Relinquished")) %>% 
      distinct() %>% 
      mutate(relinq_team = ifelse(str_detect(Notes, "trade") & !str_detect(Notes, "\\d.*team trade"),
                                  str_replace_all(Notes, "trade with ", ""),
                                  relinq_team)) %>% 
      # TODO: Fix this; right now there are some observations with unknown relinquished teams (~24)
      mutate(relinq_team = ifelse(str_detect(Notes, "trade") & is.na(relinq_team),
                                  "Unknown Team",
                                  relinq_team)) %>% 
       
      mutate(edge_label = ifelse(!is.na(relinq_team), 
                                 str_replace(edge_label, "\nacquire", paste0("\n", relinq_team, " acquire")),
                                 edge_label)) %>% 
      
      mutate(edge_label = ifelse(str_detect(Notes, "\\d.*team trade with"),
                                 str_replace_all(
                                   paste0(edge_label, 
                                          "\nOther Team(s) Involved in Trade: ",
                                          str_replace_all(
                                            str_replace_all(
                                              str_replace_all(Notes, "\\d.*team trade with ", ""), 
                                              Team, ""),
                                            relinq_team, "")),
                                   ", $", ""),
                                 edge_label),
             edge_label = str_replace(edge_label, "trade with", ""),
             edge_label = str_replace_all(edge_label, " , ", ""),
             edge_label = str_trim(edge_label))
  }
  make_edge_label <- function(df) {
    df %>% 
      mutate(edge_label = case_when(str_detect(Notes, "trade") ~ sprintf("On %s: \nacquire %s \n%s acquire %s",
                                                                         Date, 
                                                                         Relinquished,
                                                                         Team, Acquired),
                                    str_detect(Notes, "^signed.*free agent") |
                                      str_detect(Notes, "claimed off waivers") ~ sprintf("On %s, %s is %s by %s",
                                                                                         Date, Acquired, 
                                                                                         str_replace(Notes, "signed", "signed as"), 
                                                                                         Team),
                                    str_detect(Notes, "\\d{4} NBA draft.*round pick.*") |
                                      str_detect(Notes, "^first round pick") |
                                      str_detect(Notes, "^second round pick") ~ sprintf("On %s, %s pick %s with the %s", Date, Team, Acquired, Notes),
                                    str_detect(Notes, "waived") ~ sprintf("On %s, %s is waived by %s",
                                                                          Date, Relinquished, Team),
                                    str_detect(Notes, "player became.*free agent") ~ sprintf("On %s, %s becomes a free agent from %s",
                                                                          Date, Relinquished, Team),
                                    str_detect(Notes, "contract expired") ~ sprintf("On %s, %s's contract with %s expired.",
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
                      Relinquished != "free agency" & 
                      Relinquished != "draft" ~ str_replace_all(Notes,
                                                                      ".*with (.+)",
                                                                      str_c(Team, "\\1", sep = ", ")),
                    Acquired == "free agency" | Relinquished == "free agency" | Relinquished == "draft" ~ Team))
  }
  misc_cleanup <- function(df) {
    # Random one offs that need to be cleaned
    df %>% 
      mutate(Acquired = ifelse(str_detect(Acquired, "Cabarrot") | str_detect(Acquired, "2016 #24"),
                               "Timothe Luwawu / Timothe Luwawu-Cabarrot",
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "Cabarrot") | str_detect(Relinquished, "2016 #24"),
                                   "Timothe Luwawu / Timothe Luwawu-Cabarrot",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(str_detect(Acquired, "lottery protected") &
                                 str_detect(Acquired, "Jamaal Tinsley"),
                               "Jamaal Tinsley",
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "lottery protected") &
                                     str_detect(Relinquished, "Jamaal Tinsley"),
                                   "Jamaal Tinsley",
                                   Relinquished)) %>% 
      
      mutate(Acquired = ifelse(str_detect(Acquired, "lottery protected") &
                                 str_detect(Acquired, "Omar Cook"),
                               "Omar Cook",
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "lottery protected") &
                                 str_detect(Relinquished, "Omar Cook"),
                               "Omar Cook",
                               Relinquished)) %>% 
      
      # Name change
      mutate(Acquired = ifelse(str_detect(Acquired, "Jumaine Jones"),
                               "Jumaine Jones",
                               Acquired),
             Relinquished = ifelse(str_detect(Relinquished, "Jumaine Jones"),
                                   "Jumaine Jones",
                                   Relinquished)) %>% 
      
      filter(Acquired != "1999 (protected top 3)",
             Relinquished != "1999 (protected top 3)") %>% 
      # Mistaken GM Chris Wallace row
      filter(!str_detect(Acquired, "GM Chris Wallace"),
             !str_detect(Relinquished, "GM Chris Wallace"))
  }
  message("Cleaning transactions")
  # browser()
  transactions_df %>% 
    filter_for_relevant_rows() %>% 
    add_unplaced_bullets() %>% 
    split_trade_rows() %>% 
    when(.remove_future_picks ~ remove_future_picks(.),
         ~ .) %>% 
    when(.remove_not_exercised_picks ~ remove_not_exercised_picks(.),
         ~ .) %>% 
    when(.remove_modification_picks ~ remove_modification_of_picks(.),
         ~ .) %>% 
    remove_non_agreements() %>% 
    remove_right_to() %>% 
    remove_agreed_to_waive() %>% 
    remove_was_removed() %>% 
    remove_unknown_signings() %>% 
    distinct() %>% 
    mutate(Acquired = str_trim(Acquired),
           Relinquished = str_trim(Relinquished)) %>% 
    clean_misc_pick_text() %>% 
    clean_agreed_to_select() %>%
    make_edge_label() %>% 
    clean_per_media_guide() %>% 
    clean_player_to_be_named() %>% 
    clean_restricted_fa_text() %>% 
    clean_pick_text() %>% 
    clean_extraneous_parens() %>% 
    clean_exception_text() %>% 
    clean_cash_text() %>% 
    clean_rights_to_text() %>% 
    adjust_label_for_relinquished_team() %>% 
    find_teams_involved() %>%
    remove_unknown_picks() %>%
    remove_cycles() %>% 
    misc_cleanup() %>% 
    filter(Acquired != "",
           Relinquished != "") %>% 
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
           to_team = Team,
           from_team = relinq_team,
           notes = Notes,
           edge_label,
           pick_involved,
           rights_involved,
           teams_involved) %>% 
    write.csv(filename, row.names = FALSE)
}

# write_out_edgelist_df("2010-01-01", "2019-02-02")

# All data since NBA ABA merger
write_out_edgelist_df("1976-11-16", "2019-02-24")
 


