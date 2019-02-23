library(dplyr)
library(igraph)
library(stringr)
library(purrr)
library(visNetwork)
library(DT)
library(shinycssloaders)
library(shinyjs)


start <- "2010-01-01"
end <- "2019-02-02"

graph_df <- NULL

ui <- fluidPage(
  titlePanel("How does a trade ripple through the NBA?"),
  helpText(paste0("Data going from ", start, " to ", end)),
  tabsetPanel(id = "tabs",
    tabPanel("Network Visualization", 
             shinycssloaders::withSpinner(visNetworkOutput("tradeNetwork"))),
    tabPanel("How does it connect?",
             sidebarLayout(uiOutput("edgeMovieOptions"),
                           shinycssloaders::withSpinner(visNetworkOutput("edgeMovie")))),
    tabPanel("Tabular Data", 
             shinycssloaders::withSpinner(dataTableOutput("tradeData"))),
    tabPanel("About", column(8, offset = 2, htmlOutput("about")))
  ),
  tags$hr(style="border-color: black;"),
  fluidRow(uiOutput("playerUI"), 
           uiOutput("teamUI"),
           uiOutput("optionsUI"))
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
  conditional_cash_filter <- function(df) {
    if (!input$includeCash) {
      df <- 
        df %>% 
        filter(from != "cash", 
               to != "cash")
    } else {
      df
    }
  }
  conditional_exception_filter <- function(df) {
    if (!input$includeException) {
      df <-
        df %>% 
        filter(from != "trade exception", 
               to != "trade exception")
    } else {
      df
    }
  }
  conditional_free_agency_filter <- function(df) {
    if (!input$includeFreeAgency) {
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
  teams_filter <- function(df) {
    if (!is.null(input$teamsRestriction)) {
      df <- 
        df %>% 
        filter(str_detect(teams_involved, 
                          paste(paste0("(", input$teamsRestriction, ")"), 
                                collapse = "|")))
    } else {
      df
    }
  }
  user_filter <- function(df) {
    df %>% 
      conditional_cash_filter() %>% 
      conditional_exception_filter() %>% 
      conditional_free_agency_filter() %>% 
      draft_filter() %>% 
      teams_filter()
  }
  
  
  # UI related functions
  check_not_about_tab <- function(ui_el) {
    conditionalPanel("input.tabs != 'About'", ui_el)
  }
  
  output$playerUI <- renderUI({
    
    sidebarPanel(
           selectizeInput(
             inputId = "players",
             label = "Trades associated with which NBA player(s)",
             choices = sort(unique(c(graph_data()[["from"]], 
                                     graph_data()[["to"]]))),
             multiple = TRUE,
             options = list(maxOptions = length(unique(c(graph_data()[["from"]], graph_data()[["to"]]))))
           ),
           conditionalPanel(
             condition = "input.players != null & !input.includeFreeAgency & input.teamsRestriction == null",
             sliderInput("numSteps", "Including other players how many exchanges away", 
                         min = 1, max = 10, value = 2))
    ) %>% 
      check_not_about_tab()
  })
  output$teamUI <- renderUI({
      sidebarPanel(
             selectInput("teamsRestriction", 
                         "Trades associated with which NBA team(s)",
                         pull(graph_data(), teams_involved) %>% 
                           str_split(", ") %>% 
                           unlist() %>% 
                           unique() %>% 
                           str_trim() %>%
                           sort(),
                         multiple = TRUE)
      ) %>% 
        check_not_about_tab()
    })
  output$optionsUI <- renderUI({
    sidebarPanel(
           conditionalPanel(
             condition = "input.teamsRestriction == null",
             checkboxInput("includeFreeAgency", "Include free agency as part of the network")
           ),
           checkboxInput("includeDraft", "Include the draft as part of the network"),
           checkboxInput("includeCash", "Include cash as a part of the network"),
           checkboxInput("includeException", "Include trade exceptions as a part of the network"),
           shiny::tags$br(),
           downloadButton("downloadTradeData", "Download Trade Data")
    ) %>% 
      check_not_about_tab()
  })
  
  # Network Visualization Tab ------------------------------------------------------------------
  # Filter player options based on user choices
  observe({
    if (!is.null(input$includeFreeAgency) & 
        !is.null(input$includeCash) & 
        !is.null(input$includeException)) {
      
      relevant_choices <-
        sort(unique(c(user_filter(graph_data())[["from"]], 
                      user_filter(graph_data())[["to"]])))
      
      updateSelectizeInput(session, 
                           inputId = "players", 
                           choices = relevant_choices,
                           selected = intersect(input$players, relevant_choices))
    }
    
  })
  
  
  
  # Graph-related creation functions
  make_subgraph <- function(g, players, order) {
    subgraph_ids <-
      ego(g, order = order, nodes = players) %>% 
      unlist()
    
    induced_subgraph(g, subgraph_ids)
  }
  
  conditional_add_draft_to_edges_df <- function(edges_df, g) {
    if (input$includeDraft) {
      draft_df <-
        graph_data() %>% 
        filter(from == "draft",
               to %in% as_data_frame(g, "vertices")[["name"]]) %>% 
        select(from, to)
      
      edges_df <- bind_rows(edges_df, draft_df)
    } else {
      edges_df
    }
  }
  add_edge_labels <- function(edges_df) {
    edges_df %>% 
      # TODO: Currently hacky fix to get edge labels, make better
      left_join(graph_data() %>% select(from, to, 
                                        title = edge_label, 
                                        pick_involved, 
                                        rights_involved),
                by = c("from", "to")) %>% 
      left_join(graph_data() %>% select(to = from, from = to, 
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
  
  make_visNodes <- function(g) {
    nodes_df <- as_data_frame(g, "vertices")
    if (input$includeDraft) {
      nodes_df <- 
        bind_rows(nodes_df,
                  data.frame(name = "draft", stringsAsFactors = FALSE))
    }
    nodes_df %>% 
      rename(id = name) %>% 
      mutate(title = id,
             title = str_wrap(title, width = 40),
             title = str_replace_all(title, "\n", "<br>"),
             label = "")
  }
  make_visEdges <- function(g) {
    as_data_frame(g, "edges") %>% 
      conditional_add_draft_to_edges_df(g) %>% 
      add_edge_labels() %>% 
      add_edge_options() %>% 
      format_edge_labels()
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
  prepare_rendered_graph <- function() {
    igraph_edgelist <-
      graph_data() %>% 
      user_filter() %>% 
      select(from, to, date)
    
    if (length(input$teamsRestriction) > 0) {
      igraph_edgelist <-
        igraph_edgelist %>% 
        filter(from != "free agency", to != "free agency")
    }
    
    full_igraph <- graph_from_data_frame(igraph_edgelist, directed = TRUE) 
    
    if (!input$includeFreeAgency)  {
      numSteps <- input$numSteps
    } else {
      numSteps <- 1
    }
    
    if (!is.null(input$players)) {
      make_subgraph(full_igraph, input$players, numSteps)
    } else {
      full_igraph
    }
  }
  create_edge_legend_df <- function() {
    data.frame(color = c("#6BFFC1", "#997E57", "#0ECC47", "#CC7E0E"),
               label = c('"Normal Trade"', "Rights Involved", "Pick Involved", "Rights & Pick Involved"),
               width = 6,
               font.align = "top",
               stringsAsFactors = FALSE)
  }
  format_network_viz <- function(visNetworkObj, edge_legend_df) {
    visNetworkObj %>% 
      visIgraphLayout(randomSeed = 123) %>% 
      visNodes(label = " ", color = "#FF6B2B", shape = "circle") %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
                 nodesIdSelection = TRUE) %>% 
      visLegend(addEdges = edge_legend_df)
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
  
  output$tradeNetwork <- renderVisNetwork({
    if (!is.null(input$players) | !is.null(input$teamsRestriction)) {
      
      vis_subgraph <- prepare_rendered_graph()
      
      ledges <- create_edge_legend_df()
      nodes_df <- make_visNodes(vis_subgraph)
      edges_df <- make_visEdges(vis_subgraph)
      
      visNetwork(nodes_df, edges_df) %>% 
        format_network_viz(ledges) %>% 
        adjust_viz_if_multiple_edges(edges_df)
    }
  })
  
  # Edge Movie Tab -------------------------------------------------------------------------
  # TODO: Refactor all code to isolates/reactives
  # TODO: Do the edge movie by time
  # TODO: Arrange nodes in stable and nice positions
  # TODO: Make sure you are actually allowed to pick the Related To players
  conditional_not_team_panel <- function(ui_element) {
    conditionalPanel("input.teamsRestriction == null", ui_element)
  }
  output$edgeMovieOptions <- renderUI({
    sidebarPanel(width = 3,
      selectizeInput("startPlayerPath",
                     "How does a trade with",
                     input$players),# %>% conditional_not_team_panel(),
      selectizeInput("endPlayerPath",
                     "Relate to ",
                     NULL) ,#%>% conditional_not_team_panel(),
      sliderInput("edgeMovieSlide", "",
                  value = 1,
                  step = 1,
                  min = 1,
                  max = length(edge_movie()),
                  animate = animationOptions(interval = 200,
                                             loop = FALSE, 
                                             playButton = "Go", pauseButton = "Pause"))
    )
  })
  observe({
    if (!is.null(input$players)) {
      message("Related To observer triggered")
      endPlayerOptions = setdiff(sort(V(prepare_rendered_graph())$name), 
                                 input$players)
      
      message("Current end player movie: ", input$endPlayerPath)
      message("Updated end player movie: ", input$endPlayerOptions)
      updateSelectizeInput(session, 
                           inputId = "endPlayerPath", 
                           choices = endPlayerOptions)  
    }
  })
  
  create_edge_movie_networks <- function() {
    make_visTitle <- function(edges_df) {
      edges_df %>% 
        add_edge_labels() %>% 
        format_edge_labels() %>% 
        rename(label = title) %>% 
        pull(label) %>% 
        paste(collapse = "<br><br>")
    }
    if (!is.null(input$teamsRestriction)) {
      orig_g <- prepare_rendered_graph()
      E(orig_g)$date_int <- as.numeric(factor(as.Date(E(orig_g)$date)))
      lapply(1:length(unique(E(orig_g)$date_int)), function(dt) {
        edge_subgraph <-
          orig_g %>% 
          delete_edges(which(E(orig_g)$date_int != dt)) %>% 
          delete_vertices(degree(.) == 0)
        
        nodes_df <- 
          as_data_frame(edge_subgraph, "vertices") %>% 
          rename(id = name) %>% 
          mutate(label = id,
                 label = str_wrap(label, width = 40),
                 label = str_replace_all(label, "\n", "<br>"))
        
        edges_df <- make_visEdges(edge_subgraph)
        visNetwork(nodes_df, edges_df,
                   main = paste0("Trades on ",
                                 unique(E(orig_g)[which(E(orig_g)$date_int == as.character(dt))]$date))) %>% 
          visIgraphLayout()
      })
      
    } else if (!is.null(input$startPlayerPath) & !is.null(input$endPlayerPath)) {
      
      viz_path <- 
        shortest_paths(prepare_rendered_graph(), 
                       input$startPlayerPath, 
                       input$endPlayerPath,
                       mode = "all")$vpath[[1]]$name
      
      set.seed(123)
      lapply(1:(length(viz_path)-1), function(i) {
        
        edge_subgraph <- induced_subgraph(prepare_rendered_graph(), 
                                          viz_path[c(1:(i+1))])
        
        nodes_df <- 
          as_data_frame(edge_subgraph, "vertices") %>% 
              rename(id = name) %>% 
              mutate(label = id,
                     label = str_wrap(label, width = 40),
                     label = str_replace_all(label, "\n", "<br>"))
        
        edges_df <- as_data_frame(edge_subgraph, "edges")
        
        visNetwork(nodes_df, edges_df, main = make_visTitle(edges_df))
      })
      
    }
  }
  
  edge_movie <- reactive({
    create_edge_movie_networks()
  })
  
  output$edgeMovie <- renderVisNetwork({
    edge_movie()[[input$edgeMovieSlide]]
  })
  
  
  # Tabular Data Tab -------------------------------------------------------------
  create_trade_table <- function() {
    
    vis_subgraph <- prepare_rendered_graph()
    
    make_visEdges(vis_subgraph) %>% 
      select(`Player 1` = from,
             `Player 2` = to,
             `Description`= title) %>% 
      # TODO: Fix so description shows up better
      mutate(Description = str_replace_all(Description, "<br>", " ")) 
  }
  output$downloadTradeData <- downloadHandler(
    filename = function() {
      paste0(start, "_", end, "_trade-data.csv")
    },
    content = function(file) {
      write.csv(create_trade_table(), file, row.names = FALSE)
    })
  
  output$tradeData <- renderDataTable({
    create_trade_table() %>% 
      datatable(rownames = FALSE, filter = "top")
  })
  
  output$about <-
    shiny::renderText(paste0('<br>
                             Code: <a href="https://github.com/svitkin/bball-trade-network" target=_blank>https://github.com/svitkin/bball-trade-network</a>',
                             "<br>",
                             'Data comes from <a href="http://prosportstransactions.com/" target=_blank>Pro Sports Transactions</a>',
                             "<br><br>",
                             '<p>NBA trades are a weird, byzantine mix of cash, trade exceptions, draft picks and sometimes even players. Inspired by <a href="https://www.theringer.com/nba/2019/1/30/18202947/nba-transaction-trees" target=_blank>this article</a>, this application strives to visualize the complexity, focusing on the relationships between players arising from the trades they were exchanged in. Additionally, players can be exchanged for cash, signed from free agency, waived, etc. and these relationships are also visualized. To simplify things slightly, <em>free agency</em> is a bit of a catch-all, including claims off of waivers as well. <strong>However!</strong> Due to the increase in connections caused by including <em>free agency</em> as a node with relationships to players as they go in and out of it, it is only available as an option under certain conditions (player searches going a single transaction step).</p>'))
}

# Return a Shiny app object