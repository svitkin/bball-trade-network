library(dplyr)
library(igraph)
library(stringr)
library(visNetwork)
library(DT)

edgelist_df <- 
  read.csv("data/2015-01-01_2019-01-31_edgelist-df.csv", stringsAsFactors = FALSE) %>% 
  mutate(key = paste0(pmin(from, to), pmax(from, to)))

ui <- fluidPage(
  titlePanel("NBA Player Trade Network"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "players",
        label = "Choose NBA players whose trade partners you want to see",
        choices = sort(unique(c(edgelist_df[["from"]], edgelist_df[["to"]]))),
        multiple = TRUE
      ),
      numericInput("numSteps", "How deep do you want the connections", min = 1, max = 5, 
                   value = 2)),
      mainPanel(
        tabsetPanel(
          tabPanel("Network Visualization", visNetworkOutput("tradeNetwork")),
          tabPanel("Tabular Data", dataTableOutput("tradeData")))
    ))
)

# Define the server code
server <- function(input, output) {
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
      left_join(edgelist_df %>% select(key, title = edge_label),
                by = "key") %>% 
      distinct() %>% 
      select(-key) %>% 
      mutate(title = str_wrap(title, width = 40),
             title = str_replace_all(title, "\n", "<br>"))
  }
  
  output$tradeNetwork <- renderVisNetwork({
    req(input$players)
    
    full_igraph <- graph_from_data_frame(edgelist_df %>% select(from, to), directed = FALSE) 
    vis_subgraph <- make_subgraph(full_igraph, input$players, input$numSteps)
    
    
    visNetwork(make_visNodes(vis_subgraph),
               make_visEdges(vis_subgraph)) %>% 
      visIgraphLayout() %>% 
      visNodes(label = " ") %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = TRUE),
                 nodesIdSelection = TRUE)
  })
  
  output$tradeData <- renderDataTable({
    req(input$players)
    
    full_igraph <- graph_from_data_frame(edgelist_df %>% select(from, to), directed = FALSE) 
    vis_subgraph <- make_subgraph(full_igraph, input$players, input$numSteps)
    
    make_visEdges(vis_subgraph) %>% 
      rename(`Player 1` = from,
             `Player 2` = to,
             `Description`= title) %>% 
      mutate(Description = str_replace_all(Description, "<br>", "")) %>% 
      datatable(rownames = FALSE)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)