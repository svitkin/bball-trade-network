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
# Make edges/draft part of the network when it is chosen, not part of the edges dataframe after processing

library(dplyr)
library(igraph)
library(stringr)
library(purrr)
library(visNetwork)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shiny)


source("R/network-search-functions.R")

start <- "1976-11-16"
end <- "2019-02-24"

graph_df <- NULL

ui <- fluidPage(
  titlePanel("How does a trade ripple through the NBA?"),
  helpText(paste0("Data going from ", start, " to ", end)),
  tags$hr(style="border-color: black;"),
  useShinyjs(),
  tabsetPanel(id = "tabs",
              tabPanel("Introduction",
                       shiny::tags$br(),
                       fluidRow(column(width = 10, htmlOutput("intro")))),
              tabPanel("Setup",
                       tags$head(tags$style(type="text/css", "
                                            #loadmessage {
                                            padding: 5px 0px 5px 0px;
                                            text-align: center;
                                            font-weight: bold;
                                            font-size: 100%;
                                            color: #fff;
                                            background-color: #007BA7;
                                            z-index: 105;
                                            }
                                            .no-display {
                                            display: none;
                                            }
                                            ")),
                       shiny::tags$br(),
                       conditionalPanel(condition="$('html').hasClass('shiny-busy') && !$('#loadmessage').hasClass('no-display')",
                                        tags$div(shiny::HTML("Network is being calculated...<br>
                                                 Try changing the <em>number of exchanges away</em> slider for smaller visualizations."),id="loadmessage", class = "no-display")),
                       
                       
                       fluidRow(uiOutput("playerUI")), 
                       shiny::tags$br(),
                       fluidRow(uiOutput("optionsUI"))),
              tabPanel("Network Visualization", 
                       shiny::tags$br(),
                       conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                        fluidRow(column(6, offset = 1, uiOutput("networkUI"))),
                                        shinycssloaders::withSpinner(visNetworkOutput("tradeNetwork")))),
              tabPanel("Raw Data",
                       shiny::tags$br(),
                       downloadButton("downloadTradeData", "Download Trade Data"),
                       shiny::tags$br(),
                       shiny::tags$br(),
                       shinycssloaders::withSpinner(DT::DTOutput("tradeData")))) 
)

# Define the server code
server <- function(input, output, session) {
  free_agency_filter <- function(df) {
    df %>% 
      filter(from != "free agency", 
             to != "free agency")
    
  }
  draft_filter <- function(df) {
    df %>% 
      filter(from != "draft",
             to != "draft")
  }
  
  # Underlying data manipulation functions
  graph_data <- reactive({
    read.csv(file.path("data", 
                       paste0(start, "_", end, "_edgelist-df.csv")),
             stringsAsFactors = FALSE) %>% 
      select(from, to, date, edge_label, pick_involved, rights_involved, from_team, to_team)
      
  })
             
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
  user_filter_data <- reactive({
    message("Data being user filtered again")
    graph_data() %>% 
      free_agency_filter() %>% 
      draft_filter() %>% 
      conditional_cash_filter(input$includeCash) %>% 
      conditional_exception_filter(input$includeException)
  })
  
  full_network <- reactive({
    graph_data() %>% 
      graph_from_data_frame(directed = TRUE)
  })
  
  # UI related functions
  output$playerUI <- renderUI({
    message("Player UI triggered")
    column(width = 6,
           div(style="display:inline-block",
               selectizeInput(
                 inputId = "player1choice",
                 label = "How are trades associated with",
                 choices = c(NULL),
                 options = list(maxOptions = nrow(graph_data()))
               )),
           div(style="display:inline-block",
               selectizeInput(
                 inputId = "player2choice",
                 label = "Related to",
                 choices = c("Anyone"),
                 multiple = TRUE,
                 options = list(maxOptions = nrow(graph_data()))
               ))
    )
  })
  
  output$optionsUI <- renderUI({
    column(width = 6,
           conditionalPanel(
             condition = "!input.includeFreeAgency",
             sliderInput("numSteps", "Include other players how many exchanges away", 
                         min = 1, max = 10, value = 1)),
           checkboxInput("includeDraft", "Include the draft as part of the network"),
           checkboxInput("includeFreeAgency", "Include free agency as part of the network"),
           checkboxInput("includeCash", "Include cash as part of the network"),
           checkboxInput("includeException", "Include trade exceptions as part of the network"))
  })
  
  output$networkUI <- renderUI({
    conditionalPanel("input.player2choice != null",
                     sliderTextInput("edgeMovieSlide", 
                                     label = "Stepping through time",
                                     choices = "None",
                                     animate = animationOptions(interval = 1000,
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
  
  all_p1_paths <- reactive({
    date_ego(g = full_network(), 
             gdf = user_filter_data(),
             player1 = input$player1choice,
             numSteps = chooseNumSteps())
  })
  
  nodes_from_paths <- reactive({
    unique(unlist(all_p1_paths()))
  })
  p1_network <- reactive({
    # Set up empty network
    g <-
      make_empty_graph(length(nodes_from_paths())) %>% 
      set_vertex_attr("name", value = nodes_from_paths()) 
    
    # Add each path that was found using date_ego()
    for (path in all_p1_paths()) {
      if (class(path) == "list") {
        for (p in path) {
          g <- g + path(p)
        }
      } else {
        g <- g + path(path)
      }
      
    }
    
    # Add other information from original data to graph
    gdf <-
      g %>% 
      as_data_frame("edges")
    
    first_pass_df <-
      gdf %>% 
      left_join(graph_data(),
                by = c("from", "to")) %>% 
      filter(!is.na(date))
    
    if (nrow(first_pass_df) > 0) {
      full_df <-
        bind_rows(first_pass_df,
                  gdf %>% 
                    left_join(graph_data() %>% rename(to = from, from = to),
                              by = c("from", "to")) %>%
                    filter(!is.na(date))
        )
    } else {
      full_df <-
        gdf %>% 
        left_join(graph_data() %>% rename(to = from, from = to),
                  by = c("from", "to")) %>%
        filter(!is.na(date))
    }
    
    # Return distinct graph with all edge and node information
    full_df %>% 
      mutate(key = paste0(pmin(from, to),
                          pmax(from, to),
                          date)) %>% 
      distinct(key, .keep_all = TRUE) %>% 
      graph_from_data_frame()
  })
  
  p1p2_network <- reactive({
    if (any(input$player2choice == "Anyone")) {
      p1_network()
    } else {
      # Find nodes associated with paths to player 2
      all_nodes <-
        all_p1_paths()[input$player2choice] %>% 
        unlist() %>% 
        unique()
      
      # Subset player 1's full graph to just those nodes associated with player 2
      induced_subgraph(p1_network(), all_nodes)
    }
  })
  
  # Observer to change player1 based on changing options
  observe({
    if (!is.null(input$includeFreeAgency) &
        !is.null(input$includeCash) &
        !is.null(input$includeException) &
        !is.null(input$numSteps) &
        !is.null(input$includeDraft)) {

      user_filtered_data <- user_filter_data()

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
                               choices = player1options,
                               selected = player1options[1])
        }
      } else {
        updateSelectizeInput(session,
                             inputId = "player1choice",
                             choices = player1options,
                             selected = player1options[1])
      }

    }
  }, priority = 10000)
  
  #Observer to change player2 based on player1 choice
  observe({
    if (!is.null(input$includeFreeAgency) & 
        !is.null(input$includeCash) & 
        !is.null(input$includeException) & 
        !is.null(input$numSteps) &
        !is.null(input$includeDraft) &
        !is.null(input$player1choice)) {
      
      if (input$player1choice != "") {
        shinyjs::disable("player2choice")
        player2options <- nodes_from_paths()
        
        if (input$player1choice %in% player2options) {
          
          player2options <-
            player2options %>% 
            setdiff(input$player1choice) %>% 
            sort() %>% 
            c("Anyone", .)
          
          # Get rid of Anyone option if potential network is too large to display
          if (length(player2options) > 250) {
            player2options <- setdiff(player2options, "Anyone")
          }
          
          player2choice <- isolate(input$player2choice)
          # if (input$includeDraft & !"draft" %in% player2choice & !"Anyone" %in% player2choice) {
          #   player2choice <- c(player2choice, "draft")
          # }
          # if (input$includeFreeAgency & !"free agency" %in% player2choice & !"Anyone" %in% player2choice) {
          #   player2choice <- c(player2choice, "free agency")
          # }
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
          
          shinyjs::enable("player2choice")
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
             to %in% igraph::as_data_frame(g, "vertices")[["name"]]) %>% 
      select(from, to, date, edge_label, pick_involved, rights_involved)
    
    edges_df <- bind_rows(edges_df, draft_df)
    
  }
  add_fa_to_edges_df <- function(edges_df, g) {
    g_verts <- igraph::as_data_frame(g, "vertices")[["name"]]
    fa_df <-
      graph_data() %>% 
      filter((from == "free agency" & to %in% g_verts) |
               (from %in% g_verts & to == "free agency")) %>% 
      select(from, to, date, edge_label, pick_involved, rights_involved)
    
    edges_df <- bind_rows(edges_df, fa_df)
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
  
  make_visEdges <- function(g) {
    igraph::as_data_frame(g, "edges") %>% 
      rename(title = edge_label) %>% 
      add_edge_options() %>% 
      format_edge_labels()
  }
  adjust_viz_if_multiple_edges <- function(visNetworkObj, edges_df) {
    if (check_for_multiple_edges(edges_df)) {
      visNetworkObj %>% 
        visEdges(smooth = list(enabled = TRUE)) %>% 
        visPhysics(solver = "barnesHut", barnesHut = list(springConstant = 0.002)) %>% 
        visLayout(randomSeed = 123)
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
        visIgraphLayout(layout = "layout_nicely", randomSeed = 123)
    }
    edge_legend_df <-
      data.frame(color = c("#6BFFC1", "#997E57", "#0ECC47", "#CC7E0E"),
                 label = c('Player Exchange', "Rights Involved", "Pick Involved", "Rights & Pick Involved"),
                 width = 6,
                 font.align = "top",
                 stringsAsFactors = FALSE)
    
    viz %>% 
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = list(values = sort(unique(nodes_df[["id"]])),
                                         main = "Select by player")) %>% 
      adjust_viz_if_multiple_edges(edges_df) %>% 
      visLegend(addEdges = edge_legend_df,
                width = 0.15,
                position = "right")
      
  }
  change_nodes_base_on_type <- function(nodes_df) {
    nodes_df %>% 
      mutate(size = ifelse(label %in% c("draft", "free agency", "cash", "trade exception"),
                           30,
                           25),
             color = case_when(label == "draft" ~ "lightblue",
                               label == "free agency" ~ "darkred",
                               label == "cash" ~ "green",
                               label == "trade exception" ~ "purple",
                               TRUE ~ "#FF6B2B")) %>% 
      mutate(label = ifelse(label %in% c("draft", "free agency", "cash", "trade exception"),
                            str_to_upper(label),
                            label))
  }
  edge_movie_network <- reactive({
    
    # Add draft or free agency nodes and edges based on user's choices
    player_network <- 
      p1p2_network() %>% 
      igraph::as_data_frame("edges") %>% 
      purrr::when(input$includeDraft ~ (.) %>% add_draft_to_edges_df(p1p2_network()),
                  input$includeFreeAgency ~ (.) %>% add_fa_to_edges_df(p1p2_network()),
                  ~ (.)) %>% 
      graph_from_data_frame()
      
    all_dates <- sort(unique(E(player_network)$date))
    
    # For each distinct date found in the player 1 - player 2 network
    # Subset the graph to that date and store visualization of that graph as value to date key
    date_networks <-
      lapply(1:length(all_dates), function(dt_index) {
        edge_subgraph <-
          player_network %>% 
          delete_edges(which(!E(player_network)$date %in% all_dates[1:dt_index])) %>% 
          delete_vertices(degree(.) == 0)
        
        edges_df <- 
          make_visEdges(edge_subgraph)
        
        nodes_df <-
          data.frame(id = unique(c(edges_df[["from"]], 
                                   edges_df[["to"]])),
                     stringsAsFactors = FALSE) %>% 
          mutate(label = id,
                 label = str_wrap(label, width = 40)) %>% 
          change_nodes_base_on_type()
        
        layout_viz(nodes_df, 
                   edges_df, 
                   paste0("Trades up to: ", all_dates[dt_index],
                          "<br>For the network between ", input$player1choice, " and ",
                          str_replace_all(str_wrap(paste(input$player2choice, 
                                                         collapse = ", "),
                                                   width = 40),
                                          "\n",
                                          "<br>")))
      })
    
    names(date_networks) <- all_dates
    date_networks
  })
  
  # Update date slider text 
  observe({
    if (!is.null(input$player1choice) & !is.null(input$player2choice) &
        input$tabs == "Network Visualization") {
      
      input$includeDraft
      input$numSteps
      input$includeFreeAgency
      input$includeCash
      input$includeException
      updateSliderTextInput(session,
                            "edgeMovieSlide",
                            choices = names(edge_movie_network()))  
    }
  })
  
  # If the number of steps chosen exceeds 3 then display loading message to user
  # TODO: Currently hacky solution
  observe({
    if (!is.null(input$numSteps)) {
      shinyjs::toggleClass("loadmessage", "no-display", chooseNumSteps() <= 3)
    }
  })
  
  # If any options change then allow loading message to show
  observe({
    input$player2choice
    input$includeDraft
    input$includeFreeAgency
    input$includeCash
    input$includeExceptions

    shinyjs::addClass("loadmessage", "no-display")
  })
  
  # Show date slider only when there is more than one network to show
  observe({
    if (!is.null(input$edgeMovieSlide)) {
      if (input$edgeMovieSlide == "None" | length(input$edgeMovieSlide) == 0) {
        shinyjs::hide("networkUI")
      } else {
        shinyjs::show("networkUI")
      }
    }
  })
  
  
  output$tradeNetwork <- renderVisNetwork({
    if (!is.null(input$player1choice) & !is.null(input$player2choice)) {
      if (length(edge_movie_network()) == 1) {
        edge_movie_network()[[1]] 
      } else {
        if (!is.null(input$edgeMovieSlide)) {
          edge_movie_network()[[input$edgeMovieSlide]]
        }
      }
    } else {
      # TODO: make sure this is being run when either player choice is null
      visNetwork(nodes = data.frame(id = 1,
                                    label = "CHOOSE AN OPTION FOR CONNECTED PLAYER(S)\n(in both player dropdowns)"),
                 edges = data.frame(from = c(1),
                                    to = c(0)))
    }
  })
  
  # Table section ---------------------------------------------------------------------------
  trade_table <- reactive({
    p1p2_network() %>% 
      igraph::as_data_frame("edges") %>% 
      purrr::when(input$includeDraft ~ (.) %>% add_draft_to_edges_df(p1p2_network()),
                  input$includeFreeAgency ~ (.) %>% add_fa_to_edges_df(p1p2_network()),
                  ~ (.)) %>% 
      mutate(Description = str_replace(edge_label, "On.*\n", ""),
             Description = str_replace_all(Description, "[ ]*\n", "; ")) %>% 
      select(Date = date,
             `Player 1` = from,
             `Player 2` = to,
             Description)
  })
  output$tradeData <- renderDT({
    trade_table() %>% 
      datatable(rownames = FALSE, 
                filter = "top", 
                caption = paste0("All Trade Data for ", input$player1choice))
  })
  output$downloadTradeData <- downloadHandler(
    filename = function() {
      paste0(input$player1choice, "_", 
             paste(input$player2choice, collapse = "-"), 
             "_trade-data.csv")
    },
    content = function(file) {
      write.csv(trade_table(), file, 
                row.names = FALSE)
    })
  
  
  # About Section ---------------------------------------------------------------------------
  output$intro <-
    shiny::renderText(paste0('<h4>Welcome!</h4>',
                             "<p>Over time, a Vince Carter trade away from the Raptors turns into Luke Ridnour. A Lebron James trade to the Heat brings Luke Walton to the Cavaliers a few years later. NBA trades are a weird, byzantine mix of cash, trade exceptions, draft picks and sometimes even players. Inspired by <a href='https://www.theringer.com/nba/2019/1/30/18202947/nba-transaction-trees' target=_blank>this article</a>, this application strives to visualize the complexity, focusing on the relationships between players arising from the trades they were exchanged in. In the <strong>Setup</strong> tab, choose a player who's trades you are interested in, and see how their trades turn into any other player they are connected to. Go over to the <strong>Network Visualization</strong> tab to see the result of your search and step through the relevant trades over time. See and download the raw data from the visualization in the <strong>Raw Data</strong> tab.</p>", 
                             '<p>Players can also be exchanged for cash, signed from free agency, waived, etc. and these relationships are also visualized. To simplify things slightly, <em>free agency</em> is a bit of a catch-all, including claims off of waivers as well. <strong>However!</strong> Due to the increase in connections caused by including <em>free agency</em> as a node with relationships to players as they go in and out of it, including it sets the <em>number of exchanges away</em> slider automatically to 1.</p>',
                             'Code: <a href="https://github.com/svitkin/bball-trade-network" target=_blank>https://github.com/svitkin/bball-trade-network</a>',
                             "<br>",
                             'Data comes from <a href="http://prosportstransactions.com/" target=_blank>Pro Sports Transactions</a>'))

}


# Return a Shiny app object
shinyApp(ui = ui, server = server)