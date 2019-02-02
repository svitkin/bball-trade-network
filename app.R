library(dplyr)
library(igraph)
library(stringr)
library(visNetwork)
library(DT)
library(shinycssloaders)


start <- "2010-01-01"
end <- "2019-02-01"

graph_df <- NULL

ui <- fluidPage(
  titlePanel("NBA Player Trade Network"),
  helpText(paste0("Data going from ", start, " to ", end)),
  sidebarLayout(
    uiOutput("sideBar"),
    mainPanel(
      tabsetPanel(
        tabPanel("Network Visualization", 
                 shinycssloaders::withSpinner(visNetworkOutput("tradeNetwork"))),
        tabPanel("Tabular Data", 
                 shinycssloaders::withSpinner(dataTableOutput("tradeData"))),
        tabPanel("About", htmlOutput("about")))
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
      
      # Hacky fix to get edge labels
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
      mutate(title = ifelse(is.na(title), title2, title),
             pick_involved = ifelse(is.na(pick_involved), pick_involved2, pick_involved),
             rights_involved = ifelse(is.na(rights_involved), rights_involved2, rights_involved)) %>% 
      select(-title2, -pick_involved2, -rights_involved2) %>% 
      
      # Options on edges
      mutate(color = case_when(pick_involved & rights_involved ~ "purple",
                               pick_involved ~ "green",
                               rights_involved ~ "darkblue",
                               TRUE ~ "lightblue")) %>% 
      mutate(width = 8) %>% 
      distinct() %>% 
      select(-pick_involved, -rights_involved) %>% 
      mutate(title = str_wrap(title, width = 40),
             title = str_replace_all(title, "\n", "<br>"))
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
  
  
  output$sideBar <- renderUI({
    sidebarPanel(
      selectizeInput(
        inputId = "players",
        label = "Choosing the NBA player(s)",
        choices = sort(unique(c(graph_data()[["from"]], 
                                graph_data()[["to"]]))),
        multiple = TRUE,
        options = list(maxOptions = length(unique(c(graph_data()[["from"]], graph_data()[["to"]]))))
      ),
      conditionalPanel(
        condition = "input.players != null & !input.includeFreeAgency",
        sliderInput("numSteps", "Going how many transaction steps", 
                    min = 1, max = 10, value = 2)),
      selectInput("teamsRestriction", 
                  "Choosing NBA team(s)",
                  pull(graph_data(), teams_involved) %>% 
                    str_split(", ") %>% 
                    unlist() %>% 
                    unique() %>% 
                    str_trim() %>%
                    sort(),
                  multiple = TRUE),
      conditionalPanel(
        condition = "input.teamsRestriction == null",
        checkboxInput("includeFreeAgency", "Including free agency as part of the network")
      ),
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
  
  prepare_rendered_graph <- function() {
    igraph_edgelist <-
      graph_data() %>% 
      user_filter() %>% 
      select(from, to)
    
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
  
  output$tradeNetwork <- renderVisNetwork({
    if (!is.null(input$players) | !is.null(input$teamsRestriction)) {
      
      vis_subgraph <- prepare_rendered_graph()
      
      ledges <- 
        data.frame(color = c("lightblue", "darkblue", "green", "purple"),
                   label = c('"Normal Trade"', "Rights Involved", "Pick Involved", "Rights & Pick Involved"),
                   width = 6,
                   font.align = "top",
                   stringsAsFactors = FALSE)
      
      nodes_df <- make_visNodes(vis_subgraph)
      edges_df <- make_visEdges(vis_subgraph)
      viz <-
        visNetwork(nodes_df, edges_df) %>% 
        visIgraphLayout(randomSeed = 123) %>% 
        visNodes(label = " ") %>% 
        visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
                   nodesIdSelection = TRUE)
      
      if (check_for_multiple_edges(edges_df)) {
        viz %>% 
          visEdges(smooth = list(enabled = TRUE)) %>% 
          visPhysics(solver = "barnesHut", barnesHut = list(springConstant = 0.002)) %>% 
          visLegend(addEdges = ledges)
      } else {
        viz %>% visLegend(addEdges = ledges)
      }
      
    }
    
  })
  
  create_trade_table <- function() {
    
    vis_subgraph <- prepare_rendered_graph()
    
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
  
  output$about <-
    shiny::renderText(paste0('Code: <a href="https://github.com/svitkin/bball-trade-network" target=_blank>https://github.com/svitkin/bball-trade-network</a>',
                             "<br>",
                             'Data comes from <a href="http://prosportstransactions.com/" target=_blank>Pro Sports Transactions</a>',
                             "<br><br>",
                             '<p>NBA trades are a weird, byzantine mix of cash, trade exceptions, draft picks and sometimes even players. Inspired by <a href="https://www.theringer.com/nba/2019/1/30/18202947/nba-transaction-trees" target=_blank>this article</a>, this application strives to visualize the complexity, focusing on the relationships between players arising from the trades they are involved in together. Additionally, players can be exchanged for cash, signed from free agency, waived, etc. and these relationships are also visualized. <strong>However!</strong> Due to the increase in connections caused by including <em>free agency</em> as a node with relationships to players as they go in and out of it, it is only available as an option under certain conditions.</p>'))
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)