# New app.R

# UI -------------------------------
# How are trades with player ______ (selectize input of all players in data)
# Related to ________ (selectize input of "Anyone" option all players in data {n steps} away from 1st player choice)
# n_steps slider spread
# Other options 

# Observers -----------------------------
# For changes in "related to" (2nd) player based on 1st player choice
# For changes in any UI options, the underlying graph generated
  # Create functions that parameterize reactive elements
  # Run those functions with reactive elements as parameters in explicit observers, reactive elements, etc.

# Visualization -------------------
# Network:
# If Anyone is chosen:
  # Create ego based network from player, going {n_steps}
  # Display it with visNetwork
# Else:
  # Find the path(s) between 1st player and 2nd player chosen
  # Organize each connection by time
  # Add slider that has the time steps
  # Starting with the first time step edge as a visNetwork visualization
  # As the slider increases go to that step, keeping the edges from the last time step
    # Make sure visualization is centered in this case

# Table:
  # Datatable output based on graph generated from visualization

# TODO:
# Deal with the slider single length edge movies 
# Make draft part of the network when it is chosen, not part of the edges dataframe after processing

library(dplyr)
library(igraph)
library(stringr)
library(purrr)
library(visNetwork)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)


start <- "1976-11-16"
end <- "2019-02-24"

graph_df <- NULL

ui <- fluidPage(
  titlePanel("How does a trade ripple through the NBA?"),
  helpText(paste0("Data going from ", start, " to ", end)),
  tags$hr(style="border-color: black;"),
  tabsetPanel(id = "tabs",
              tabPanel("Setup",
                       shiny::tags$br(),
                       fluidRow(uiOutput("playerUI")), 
                       fluidRow(column(width = 11, uiOutput("optionsUI"))),
                       fluidRow(column(width = 10, htmlOutput("about")))),
              tabPanel("Network Visualization", 
                       shiny::tags$br(),
                       fluidRow(column(6, offset = 1, uiOutput("networkUI"))),
                       shinycssloaders::withSpinner(visNetworkOutput("tradeNetwork"))),
              tabPanel("Raw Data",
                       shinycssloaders::withSpinner(DTOutput("tradeData")))) 
)

# Define the server code
server <- function(input, output, session) {
  
  # Underlying data manipulation functions
  graph_data <- function() {
    if (is.null(graph_df)) {
      graph_df <<-
        read.csv(file.path("data", 
                           paste0(start, "_", end, "_edgelist-df.csv")), 
                 stringsAsFactors = FALSE)
    }
    
    graph_df
  }
  conditional_cash_filter <- function(df, cashConditional) {
    if (!cashConditional) {
      df <- 
        df %>% 
        filter(from != "cash", 
               to != "cash")
    } else {
      df
    }
  }
  conditional_exception_filter <- function(df, exceptionConditional) {
    if (!exceptionConditional) {
      df <-
        df %>% 
        filter(from != "trade exception", 
               to != "trade exception")
    } else {
      df
    }
  }
  conditional_free_agency_filter <- function(df, freeagencyConditional) {
    if (!freeagencyConditional) {
      df <- 
        df %>% 
        filter(from != "free agency", 
               to != "free agency")
    } else {
      df
    }
    
  }
  draft_filter <- function(df) {
    df %>% 
      filter(from != "draft",
             to != "draft")
  }

  user_filter <- function(df) {
    df %>% 
      conditional_cash_filter(isolate(input$includeCash)) %>% 
      conditional_exception_filter(isolate(input$includeException)) %>% 
      conditional_free_agency_filter(isolate(input$includeFreeAgency)) %>% 
      draft_filter()
  }
  
  # UI related functions
  output$playerUI <- renderUI({
    
    if (!is.null(input$includeFreeAgency) &
        !is.null(input$includeDraft) &
        !is.null(input$includeCash) &
        !is.null(input$includeException)) {
      
      filtered_df <- 
        graph_data() %>% 
        user_filter()
      
      all_players <-
        unique(c(filtered_df[["from"]], 
                 filtered_df[["to"]]))
      
      column(width = 6,
             div(style="display:inline-block",
                 selectizeInput(
                   inputId = "player1choice",
                   label = "How are trades associated with",
                   choices = sort(all_players),
                   options = list(maxOptions = length(all_players))
                 )),
             div(style="display:inline-block",
                 selectizeInput(
                   inputId = "player2choice",
                   label = "Related to",
                   choices = c("Anyone"),
                   multiple = TRUE,
                   options = list(maxOptions = length(all_players))))
      )
        
    }
    
  })
  
  output$optionsUI <- renderUI({
    sidebarPanel(
      conditionalPanel(
        condition = "!input.includeFreeAgency",
        sliderInput("numSteps", "Include other players how many exchanges away", 
                    min = 1, max = 10, value = 2)),
      checkboxInput("includeDraft", "Include the draft as part of the network"),
      checkboxInput("includeFreeAgency", "Include free agency as part of the network"),
      checkboxInput("includeCash", "Include cash as part of the network"),
      checkboxInput("includeException", "Include trade exceptions as part of the network"),
      shiny::tags$br(),
      downloadButton("downloadTradeData", "Download Trade Data"))
  })
  
  output$networkUI <- renderUI({
    conditionalPanel("input.player2choice != null",
                     sliderTextInput("edgeMovieSlide", 
                                     label = "Stepping through time",
                                     choices = "None",
                                     animate = animationOptions(interval = 3000,
                                                                loop = FALSE, 
                                                                playButton = "Play All",
                                                                pauseButton = "Pause")))
  })
  
  # Graph related functions
  chooseNumSteps <- reactive({
    if (!input$includeFreeAgency)  {
      input$numSteps
    } else {
      1
    }
  })
  make_full_network <- function(df) {
    df %>% 
      select(from, to, date, edge_label, pick_involved, rights_involved) %>% 
      graph_from_data_frame(directed = TRUE)
  }
  make_player_network <- function(full_network, playerChoice, numSteps, draftCondition) {
    if (!draftCondition) {
      ego(full_network,
          nodes = playerChoice,
          order = numSteps,
          mode = "all") %>% 
        unlist() %>% 
        induced_subgraph(full_network, .)  
    } else {
      nodraft_network <-
        ego(full_network,
            nodes = playerChoice,
            order = numSteps,
            mode = "all") %>% 
        unlist() %>% 
        induced_subgraph(full_network, .)
      
      nodraft_network %>% 
        as_data_frame("edges") %>% 
        add_draft_to_edges_df(nodraft_network) %>% 
        graph_from_data_frame()
    }
  }
  get_connected_players <- function(player_network) {
    V(player_network)$name
  }
  
  # Observer to change player1 based on changing options
  observe({
    if (!is.null(input$includeFreeAgency) &
        !is.null(input$includeCash) &
        !is.null(input$includeException) & 
        !is.null(input$numSteps) &
        !is.null(input$includeDraft)) {

      user_filtered_data <-
        graph_data() %>% 
        user_filter()
        
      player1options <-
        sort(unique(c(user_filtered_data[["from"]],
                      user_filtered_data[["to"]])))
      
      player1choice <- isolate(input$player1choice)
      
      if (!is.null(player1choice)) {
        if (player1choice %in% player1options) {
          updateSelectizeInput(session, 
                               inputId = "player1choice",
                               choices = player1options,
                               selected = player1choice)    
        } else {
          updateSelectizeInput(session, 
                               inputId = "player1choice",
                               choices = player1options)
        }
      } else {
        updateSelectizeInput(session, 
                             inputId = "player1choice",
                             choices = player1options)
      }
      
    }
  })
  
  #Observer to change player2 based on player1 choice
  observe({
    if (!is.null(input$includeFreeAgency) & 
        !is.null(input$includeCash) & 
        !is.null(input$includeException) & 
        !is.null(input$numSteps) &
        !is.null(input$includeDraft) &
        !is.null(input$player1choice)) {
      
      full_network <- 
        graph_data() %>% 
        user_filter() %>% 
        make_full_network()
      
      if (input$player1choice %in% V(full_network)$name) {
        player2options <-
          full_network %>% 
          make_player_network(input$player1choice, 
                              chooseNumSteps(),
                              input$includeDraft) %>% 
          get_connected_players() %>% 
          setdiff(input$player1choice) %>% 
          sort() %>% 
          c("Anyone", .)
        
        player2choice <- isolate(input$player2choice)
        if (input$includeDraft & !"draft" %in% player2choice & !"Anyone" %in% player2choice) {
          player2choice <- c(player2choice, "draft")
        }
        if (input$includeFreeAgency & !"free agency" %in% player2choice & !"Anyone" %in% player2choice) {
          player2choice <- c(player2choice, "free agency")
        }
        if (input$includeCash & !"cash" %in% player2choice & !"Anyone" %in% player2choice) {
          player2choice <- c(player2choice, "cash")
        }
        if (input$includeException & "trade exception" %in% player2choice & !"Anyone" %in% player2choice) {
          player2choice <- c(player2choice, "trade exception")
        }
        
        
        if (!is.null(player2choice)) {
          if (all(player2choice %in% player2options)) {
            updateSelectizeInput(session, 
                                 inputId = "player2choice",
                                 choices = player2options,
                                 selected = player2choice)
          } else {
            updateSelectizeInput(session, 
                                 inputId = "player2choice",
                                 choices = player2options)  
          }
        } else {
          updateSelectizeInput(session, 
                               inputId = "player2choice",
                               choices = player2options)  
        }
        
      }
        
    }
      
  })
  
  
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
  add_draft_to_edges_df <- function(edges_df, g) {
    
    draft_df <-
      graph_data() %>% 
      filter(from == "draft",
             to %in% as_data_frame(g, "vertices")[["name"]]) %>% 
      select(from, to, date, edge_label, pick_involved, rights_involved)
    
    edges_df <- bind_rows(edges_df, draft_df)
    
  }
  check_for_multiple_edges <- function(edges_df) {
    multiedges <-
      edges_df %>% 
      mutate(edgekey = paste0(pmin(from, to),
                              pmax(from, to))) %>% 
      count(edgekey) %>% 
      filter(n > 1) %>% 
      nrow()
    
    multiedges > 0
  }
  
  make_visEdges <- function(g, draftCondition) {
    as_data_frame(g, "edges") %>% 
      rename(title = edge_label) %>% 
      add_edge_options() %>% 
      format_edge_labels()
  }
  adjust_viz_if_multiple_edges <- function(visNetworkObj, edges_df) {
    if (check_for_multiple_edges(edges_df)) {
      visNetworkObj %>% 
        visEdges(smooth = list(enabled = TRUE)) %>% 
        visPhysics(solver = "barnesHut", barnesHut = list(springConstant = 0.002))
    } else {
      visNetworkObj
    }
  }
  
  # Create Edge Movie Network
  layout_viz <- function(nodes_df, edges_df, title) {
    if (nrow(nodes_df) == 2) {
      viz <- 
        visNetwork(nodes_df, edges_df, main = title)
    } else {
      viz <- 
        visNetwork(nodes_df, edges_df, main = title) %>% 
        visIgraphLayout(layout = "layout_in_circle", randomSeed = 123)
    }
    viz %>% 
      adjust_viz_if_multiple_edges(edges_df) %>% 
      visNodes(color = "#FF6B2B")
  }
  make_edge_movie_networks <- function(player_network, 
                                       player1choice, player2choices, 
                                       draftCondition, 
                                       anyoneCondition) {
    
    if (!anyoneCondition) {
      path_players <-
        unlist(shortest_paths(player_network, 
                              player1choice, 
                              player2choices,
                              mode = "all")$vpath)
      
      subgraph_paths <- induced_subgraph(player_network, path_players)  
    } else {
      subgraph_paths <- player_network
    }
    
    all_dates <- sort(unique(E(subgraph_paths)$date))
    
    date_networks <-
      lapply(1:length(all_dates), function(dt_index) {
        edge_subgraph <-
          subgraph_paths %>% 
          delete_edges(which(!E(subgraph_paths)$date %in% all_dates[1:dt_index])) %>% 
          delete_vertices(degree(.) == 0)
        
        edges_df <- make_visEdges(edge_subgraph, draftCondition)
        nodes_df <-
          data.frame(id = unique(c(edges_df[["from"]], 
                                   edges_df[["to"]])),
                     stringsAsFactors = FALSE) %>% 
          mutate(label = id,
                 label = str_wrap(label, width = 40))
        
        layout_viz(nodes_df, 
                   edges_df, 
                   paste0("Trades up to: ", all_dates[dt_index],
                          "<br>For the network between ", player1choice, " and ",
                          str_replace_all(str_wrap(paste(player2choices, 
                                                         collapse = ", "),
                                                   width = 40),
                                          "\n",
                                          "<br>")))
      })
    
    names(date_networks) <- all_dates
    date_networks
  }
  
  edge_movie <- reactive({
    if (!is.null(input$player1choice) & !is.null(input$player2choice)) {
      
      full_network <-
        graph_data() %>%
        user_filter() %>%
        make_full_network()
        
      anyoneCondition <- "Anyone" %in% input$player2choice    
      if ((input$player1choice %in% V(full_network)$name &
          anyoneCondition) |
          (input$player1choice %in% V(full_network)$name &
           input$includeDraft) |
          (input$player1choice %in% V(full_network)$name &
          all(input$player2choice %in% V(full_network)$name))) {
        
        player_network <-
          make_player_network(full_network,
                              input$player1choice,
                              chooseNumSteps(),
                              input$includeDraft)
        
        make_edge_movie_networks(player_network,
                                 input$player1choice,
                                 input$player2choice,
                                 input$includeDraft,
                                 anyoneCondition)
        
      }
    }
  })
  
  # Update edge movie slider length
  observe({
    if (!is.null(input$player1choice) & !is.null(input$player2choice) &
        input$tabs == "Network Visualization") {
      
      input$includeDraft
      input$numSteps
      input$includeFreeAgency
      input$includeCash
      input$includeException
      
      full_network <-
        graph_data() %>%
        user_filter() %>%
        make_full_network()
      
      anyoneCondition <- "Anyone" %in% input$player2choice    
      if ((input$player1choice %in% V(full_network)$name &
           anyoneCondition) |
          (input$player1choice %in% V(full_network)$name &
           input$includeDraft) |
          (input$player1choice %in% V(full_network)$name &
           all(input$player2choice %in% V(full_network)$name))) {
       
          updateSliderTextInput(session,
                                "edgeMovieSlide",
                                choices = names(edge_movie()))  
      }
    }
  })
  
  # TODO: Fix this observer so slide only shows when there is more than one network to show
  # observe({
  #   if (input$tabs == "Network Visualization") {
  #     input$includeDraft
  #     input$numSteps
  #     input$includeFreeAgency
  #     input$includeCash
  #     input$includeException
  #     input$player1choice
  #     input$player2choice
  #     message("Toggler triggered")
  #     message("Length is ", length(edge_movie()))
  #     if (length(edge_movie) <= 1) {
  #       shinyjs::hide("edgeMovieSlide")
  #     } else {
  #       shinyjs::show("edgeMovieSlide")
  #     }  
  #   }
  # })
  
  output$tradeNetwork <- renderVisNetwork({
    if (!is.null(input$player1choice) & !is.null(input$player2choice)) {
      if (length(edge_movie()) == 1) {
        edge_movie()[[1]] 
      } else {
        if (!is.null(input$edgeMovieSlide)) {
          edge_movie()[[input$edgeMovieSlide]]
        }
      }
    } else {
      visNetwork(nodes = data.frame(id = 1,
                                    label = "CHOOSE AN OPTION FOR CONNECTED PLAYER(S)\n(in both player dropdowns)"),
                 edges = data.frame(from = c(1),
                                    to = c(0)))
    }
  })
  
  # Table section ---------------------------------------------------------------------------
  create_trade_table <- function(player1, numSteps, draftCondition) {
    full_network <-
      graph_data() %>%
      user_filter() %>%
      make_full_network()
     
    if (player1 %in% V(full_network)$name) {
      player_network <-
        make_player_network(full_network,
                            player1,
                            numSteps,
                            draftCondition) 
      
      as_data_frame(player_network, "edges") %>% 
        mutate(Description = str_replace(edge_label, "On.*\n", ""),
               Description = str_replace_all(Description, "[ ]*\n", "; ")) %>% 
        select(Date = date,
               `Player 1` = from,
               `Player 2` = to,
               Description)
    }
  }
  output$tradeData <- renderDT({
    create_trade_table(input$player1choice,
                       chooseNumSteps(),
                       input$includeDraft) %>% 
      datatable(rownames = FALSE, 
                filter = "top", 
                caption = paste0("All Trade Data for ", input$player1choice))
  })
  output$downloadTradeData <- downloadHandler(
    filename = function() {
      paste0(input$player1choice, "_trade-data.csv")
    },
    content = function(file) {
      write.csv(create_trade_table(input$player1choice,
                                   chooseNumSteps(),
                                   input$includeDraft), 
                file, 
                row.names = FALSE)
    })
  
  
  # About Section ---------------------------------------------------------------------------
  output$about <-
    shiny::renderText(paste0('<br>
                              <h4>About</h4>
                             Code: <a href="https://github.com/svitkin/bball-trade-network" target=_blank>https://github.com/svitkin/bball-trade-network</a>',
                             "<br>",
                             'Data comes from <a href="http://prosportstransactions.com/" target=_blank>Pro Sports Transactions</a>',
                             "<br><br>",
                             '<p>NBA trades are a weird, byzantine mix of cash, trade exceptions, draft picks and sometimes even players. Inspired by <a href="https://www.theringer.com/nba/2019/1/30/18202947/nba-transaction-trees" target=_blank>this article</a>, this application strives to visualize the complexity, focusing on the relationships between players arising from the trades they were exchanged in. Additionally, players can be exchanged for cash, signed from free agency, waived, etc. and these relationships are also visualized. To simplify things slightly, <em>free agency</em> is a bit of a catch-all, including claims off of waivers as well. <strong>However!</strong> Due to the increase in connections caused by including <em>free agency</em> as a node with relationships to players as they go in and out of it, it is only available as an option under certain conditions (player searches going a single transaction step).</p>'))

}


# Return a Shiny app object
shinyApp(ui = ui, server = server)