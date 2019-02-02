library(dplyr)
library(igraph)
library(stringr)
library(visNetwork)
library(DT)


set.seed(123)
start <- "2010-01-01"
end <- "2019-02-01"

graph_df <- NULL

ui <- fluidPage(
  titlePanel("NBA Player Trade Network"),
  helpText(paste0("Data going from ", start, " to ", end)),
  a("Code", target="_blank", href="https://github.com/svitkin/bball-trade-network"), 
  br(),
  a("Pro Sports Transactions", target="_blank", href="http://prosportstransactions.com/"),
  br(),
  sidebarLayout(
    uiOutput("sideBar"),
    mainPanel(
      tabsetPanel(
        tabPanel("Network Visualization", visNetworkOutput("tradeNetwork")),
        tabPanel("Tabular Data", dataTableOutput("tradeData")))
    ))
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
      teams_filter()
  }
  
  # Graph-related creation functions
  make_subgraph <- function(g, players, order) {
    subgraph_ids <-
      ego(g, order = order, nodes = players) %>% 
      unlist()
    
    induced_subgraph(g, subgraph_ids)
  }
  make_visNodes <- function(g) {
    nodes_df <- as_data_frame(g, "vertices")
    nodes_df %>% 
      rename(id = name) %>% 
      mutate(title = id,
             title = str_wrap(title, width = 40),
             title = str_replace_all(title, "\n", "<br>"),
             label = "")
  }
  make_visEdges <- function(g) {
    edges_df <- as_data_frame(g, "edges")
    edges_df %>% 
      mutate(key = paste0(pmin(from, to), pmax(from, to))) %>% 
      left_join(graph_df %>% select(key, title = edge_label, pick_involved, rights_involved),
                by = "key") %>% 
      mutate(color = case_when(pick_involved & rights_involved ~ "purple",
                               pick_involved ~ "green",
                               rights_involved ~ "darkblue",
                               TRUE ~ "lightblue")) %>% 
      mutate(width = 4) %>% 
      distinct() %>% 
      select(-key, -pick_involved, -rights_involved) %>% 
      mutate(title = str_wrap(title, width = 40),
             title = str_replace_all(title, "\n", "<br>"))
  }
  
  
  
  output$sideBar <- renderUI({
    sidebarPanel(
      selectizeInput(
        inputId = "players",
        label = "Restricting NBA player(s) to",
        choices = sort(unique(c(graph_data()[["from"]], 
                                graph_data()[["to"]]))),
        multiple = TRUE,
        options = list(maxOptions = length(unique(c(graph_data()[["from"]], graph_data()[["to"]]))))
      ),
      selectInput("teamsRestriction", 
                  "Restricting to trades involving",
                  pull(graph_data(), teams_involved) %>% 
                    str_split(", ") %>% 
                    unlist() %>% 
                    unique() %>% 
                    str_trim() %>%
                    sort(),
                  multiple = TRUE),
      numericInput("numSteps", "If you chose player(s), how many transaction steps", 
                   min = 1, max = 10, value = 2),
      checkboxInput("includeFreeAgency", "Including free agency as part of the network"),
      checkboxInput("includeCash", "Including cash as a part of the network"),
      checkboxInput("includeException", "Including trade exceptions as a part of the network"),
      shiny::tags$br(),
      downloadButton("downloadTradeData", "Download Trade Data"))
    
  })
  
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
  
  output$tradeNetwork <- renderVisNetwork({
    if (!is.null(input$players) | !is.null(input$teamsRestriction)) {
      
      igraph_edgelist <-
        graph_data() %>% 
        user_filter() %>% 
        select(from, to)
      
      full_igraph <- graph_from_data_frame(igraph_edgelist, directed = FALSE)
      if (!is.null(input$players)) {
        vis_subgraph <- make_subgraph(full_igraph, input$players, input$numSteps)
      } else {
        vis_subgraph <- full_igraph
      }
      ledges <- 
        data.frame(color = c("lightblue", "darkblue", "green", "purple"),
                   label = c('"Normal Trade"', "Rights Involved", "Pick Involved", "Rights & Pick Involved"),
                   width = 6,
                   font.align = "top",
                   stringsAsFactors = FALSE)
      
      visNetwork(make_visNodes(vis_subgraph),
                 make_visEdges(vis_subgraph)) %>% 
        visIgraphLayout() %>% 
        visNodes(label = " ") %>% 
        visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
                   nodesIdSelection = TRUE) %>% 
        visLegend(addEdges = ledges)
      
    }
    
  })
  
  create_trade_table <- function() {
    igraph_edgelist <-
      graph_data() %>% 
      user_filter() %>% 
      select(from, to)
    
    full_igraph <- graph_from_data_frame(igraph_edgelist, directed = FALSE) 
    if (!is.null(input$players)) {
      vis_subgraph <- make_subgraph(full_igraph, input$players, input$numSteps)
    } else {
      vis_subgraph <- full_igraph
    }
    
    make_visEdges(vis_subgraph) %>% 
      select(`Player 1` = from,
             `Player 2` = to,
             `Description`= title) %>% 
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
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)