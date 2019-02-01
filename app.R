library(dplyr)
library(igraph)
library(stringr)
library(visNetwork)
library(DT)


set.seed(123)
start <- "2016-01-01"
end <- "2019-02-01"

edgelist_df <- 
  read.csv(file.path("data", 
                     paste0(start, "_", end, "_edgelist-df.csv")), 
           stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("NBA Player Trade Network"),
  helpText(paste0("Data going from ", start, " to ", end)),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "players",
        label = "Starting with NBA player(s)",
        choices = sort(unique(c(edgelist_df[["from"]], edgelist_df[["to"]]))),
        multiple = TRUE
      ),
      uiOutput("teams"),
      numericInput("numSteps", "Going how many transaction steps", min = 1, max = 5, 
                   value = 2),
      checkboxInput("includeFreeAgency", "Including free agency as part of the network"),
      checkboxInput("includeCash", "Including cash as a part of the network"),
      checkboxInput("includeException", "Including trade exceptions as a part of the network")),
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
      left_join(edgelist_df %>% select(key, title = edge_label, pick_involved, rights_involved),
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
  
  output$teams <- renderUI({
    selectInput("teamsRestriction", 
                "Restricting to trades involving",
                pull(edgelist_df, teams_involved) %>% 
                  str_split(", ") %>% 
                  unlist() %>% 
                  unique() %>% 
                  sort(),
                multiple = TRUE)
  })
  
  output$tradeNetwork <- renderVisNetwork({
    if (!input$includeCash) {
      igraph_edgelist <- 
        edgelist_df %>% 
        filter(from != "cash",
               to != "cash")
    } 
    if (!input$includeException) {
      igraph_edgelist <- 
        igraph_edgelist %>% 
        filter(from != "trade exception",
               to != "trade exception")
    } 
    if (!input$includeFreeAgency) {
      igraph_edgelist <-
        igraph_edgelist %>% 
        filter(from != "free agency",
               to != "free agency")
    }
    
    
    if (!is.null(input$teamsRestriction)) {
      filter_string <-paste0("str_detect(teams_involved, ", 
                             paste0("'", input$teamsRestriction, "'"), ")")
      if (length(input$teamsRestriction) > 1) filter_string <- paste(filter_string, collapse = "|")
      message(filter_string)
      igraph_edgelist <-
        igraph_edgelist %>% 
        filter_(filter_string)  
    }
    igraph_edgelist <-
      igraph_edgelist %>% 
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
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = TRUE),
                 nodesIdSelection = TRUE) %>% 
      visLegend(addEdges = ledges)
  })
  
  output$tradeData <- renderDataTable({
    if (!input$includeCash) {
      igraph_edgelist <- 
        edgelist_df %>% 
        filter(from != "cash",
               to != "cash")
    } 
    if (!input$includeException) {
      igraph_edgelist <- 
        igraph_edgelist %>% 
        filter(from != "trade exception",
               to != "trade exception")
    } 
    if (!input$includeFreeAgency) {
      igraph_edgelist <-
        igraph_edgelist %>% 
        filter(from != "free agency",
               to != "free agency")
    }
    
    
    if (!is.null(input$teamsRestriction)) {
      filter_string <-paste0("str_detect(teams_involved, ", 
                             paste0("'", input$teamsRestriction, "'"), ")")
      if (length(input$teamsRestriction) > 1) filter_string <- paste(filter_string, collapse = "|")
      message(filter_string)
      igraph_edgelist <-
        igraph_edgelist %>% 
        filter_(filter_string)  
    }
    igraph_edgelist <-
      igraph_edgelist %>% 
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
      mutate(Description = str_replace_all(Description, "<br>", " ")) %>% 
      datatable(rownames = FALSE)
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)