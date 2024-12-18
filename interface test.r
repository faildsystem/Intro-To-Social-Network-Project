# # Required packages
# 
# packages <-
#   c(
#     "shiny",
#     "shinycssloaders",
#     "igraph",
#     "tidyverse",
#     "ggplot2",
#     "reshape2",
#     "rgl",
#     "plotly",
#     "ff",
#     "dplyr",
#     "visNetwork",
#     "webshot"
# 
#   )
# 
# # Loop to install packages if they are not already installed
# 
# for (package in packages) {
#   if (!require(package, character.only = TRUE)) {
#     install.packages(package, dependencies = TRUE)
# 
#     # Load the package after installation
# 
#     library(package, character.only = TRUE)
#   }
# }
# 
# 
# webshot::install_phantomjs()

# Increase file upload size to 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)

# UI definition
ui <- fluidPage(
  titlePanel("Graph Metrics Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload CSV Files", 
                multiple = TRUE, 
                accept = c(".csv")),
      radioButtons("graphType", "Graph Type:", 
                   choices = c("Undirected" = "undirected", 
                               "Directed" = "directed"), 
                   selected = "undirected"),
      radioButtons("normalized", "Normalized Results:", 
                   choices = c("Yes" = TRUE, 
                               "No" = FALSE), 
                   selected = FALSE),
      actionButton("process", "Calculate Metrics")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graph Info", 
                 br(),
                 withSpinner(textOutput("nodesCount"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("edgesCount"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("connected"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("minDegree"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("avgDegree"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("maxDegree"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("diameter"), type = 7, color = "#0d6efd"),
                 br(),
                 withSpinner(textOutput("clustering"), type = 7, color = "#0d6efd"),
                 ),
        tabPanel("Network", 
                 downloadButton("downloadNetwork", "Download Network Image", style = "margin: 15px 0;"),
                 withSpinner(visNetworkOutput("network"), type = 7, color = "#0d6efd")
                 ),
        tabPanel("Degree Distribution",
                 withSpinner(plotlyOutput("degreeDistPlot"), type = 7, color = "#0d6efd")
                 ),
        tabPanel("Adjacency Matrix",
                 br(),
                 withSpinner(tableOutput("adjMatrix"), type = 7, color = "#0d6efd")),
        tabPanel("Centerality Metrics",
                 downloadButton("downloadMetrics", "Download Metrics", style = "margin: 15px 0;"),
                 withSpinner(tableOutput("metricsTable"), type = 7, color = "#0d6efd")),
        tabPanel("Node Metrics",
                 uiOutput("nodeSelector"),
                 actionButton("calculateNodeMetrics", "Calculate Node Metrics"),
                 tableOutput("nodeMetricsTable")
                 ),
        tabPanel("Path Between Nodes",
                 uiOutput("nodeSelectorPath"),
                 actionButton("calculatePath", "Calculate Path Between Nodes"),
                 textOutput("pathResult")
                 )
      )
    )
  )
)

# Server definition
server <- function(input, output, session) {
  
  # Reactive value to store edges and results
  edges <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  # Read and combine uploaded files
  observeEvent(input$process, {
    req(input$files)
    
    # Show a loading message
    showModal(modalDialog("Processing files...", footer = NULL, easyClose = FALSE))
    
    # Read all files into a list of data frames
    files <- lapply(input$files$datapath, read.csv)
    combined_edges <- bind_rows(files)
    
    edges(combined_edges)
    
    # Calculate metrics
    calculate_metrics <- function(edges, directed, isNormalized) {
      g <- graph_from_data_frame(edges, directed = (directed == "directed"))
      metrics <- data.frame(
        node = V(g)$name,
        degree = degree(g, mode = "all", normalized = isNormalized),
        betweenness = betweenness(g, directed = (directed == "directed"), normalized = isNormalized),
        closeness = closeness(g, normalized = isNormalized),
        clustering_coefficient = transitivity(g, type = "local", isolates = "zero")
      ) %>% 
        mutate(across(where(is.numeric), ~round(., 3)))
      
      adjacency <- as.matrix(as_adjacency_matrix(g, sparse = FALSE))
      
      
      avg_degree <- mean(degree(g, mode = "all", normalized = isNormalized))
      
      list(
        metrics = metrics,
        diameter = diameter(g, directed = (directed == "directed"), weights = NA),
        avg_clustering = transitivity(g, type = "average"),
        min_degree = min(degree(g, mode = "all", normalized = isNormalized)),
        max_degree = max(degree(g, mode = "all", normalized = isNormalized)),
        avg_degree = round(avg_degree, 3),
        connected = is_connected(g),
        nodes = vcount(g),
        edges = ecount(g),
        graph = g,
        adjacency_matrix = adjacency
      )
    }
    
    # Perform calculations
    results(calculate_metrics(edges(), input$graphType, input$normalized))
    
    # Remove loading message
    removeModal()
    
    
    # Update node selector for user input based on the graph
    nodes <- V(results()$graph)$name
    
    output$nodeSelector <- renderUI({
      selectInput("nodeIndex", "Select Node for Metrics:", choices = nodes)
    })
    
    output$nodeSelectorPath <- renderUI({
      tagList(
        selectInput("node1", "Select Node 1:", choices = nodes),
        selectInput("node2", "Select Node 2:", choices = nodes)
      )
    })
    
    # Function for calculating metrics for a specific node
    calculate_node_metrics <- function(node_name) {
      g <- results()$graph
      
      node <- match(node_name, V(g)$name)
      
      node_metrics <- results()$metrics[node, ]
      
      # Get the neighbors of the node
      neighbors <- neighbors(g, node)
      
      # Retrieve the names of the neighbors
      neighbor_names <- V(g)[neighbors]$name
      
      # Append the neighbors to the node metrics
      node_metrics$neighbors <- paste(neighbor_names, collapse = ", ")
      
      return(node_metrics)
    }
    
    # Function to calculate the path between two nodes
    calculate_path_between_nodes <- function(firstNode, secondNode) {
      g <- results()$graph
      
      firstNodeIndex <- match(firstNode, V(g)$name)
      secondNodeIndex <- match(secondNode, V(g)$name)
      
      path <- shortest_paths(g, from = firstNodeIndex, to = secondNodeIndex)$vpath[[1]]
      if (length(path) == 0) {
        return("No path found")
      } else {
        path_names <- V(g)$name[path]
        return(paste("Path: ", paste(path_names, collapse = " >>> ")))
      }
    }
    
    # Calculate node metrics when button is clicked
    observeEvent(input$calculateNodeMetrics, {
      req(input$nodeIndex)
      node_metrics <- calculate_node_metrics(input$nodeIndex)
      
      output$nodeMetricsTable <- renderTable({
        
        node_metrics
        
      })
    })
    
    # Calculate path between two nodes
    observeEvent(input$calculatePath, {
      req(input$node1, input$node2)
      path_result <- calculate_path_between_nodes(input$node1, input$node2)
      
      output$pathResult <- renderText({
        path_result
      })
    })
  })
  
  
  # Output the metrics table
  output$metricsTable <- renderTable({
    req(results())
    results()$metrics
  })
  
  # Output graph info
  output$nodesCount <- renderText({
    req(results())
    paste("Graph Nodes Number:", results()$nodes)
  })
  
  output$edgesCount <- renderText({
    req(results())
    paste("Graph Edges Number:", results()$edges)
  })
  
  # Output the min degree
  output$connected <- renderText({
    req(results())
    paste("Connected:", results()$connected)
  })
  
  # Output the min degree
  output$minDegree <- renderText({
    req(results())
    paste("Min Degree:", results()$min_degree)
  })
  
  # Output the average degree
  output$avgDegree <- renderText({
    req(results())
    paste("Average Degree:", results()$avg_degree)
  })
  
  # Output the max degree
  output$maxDegree <- renderText({
    req(results())
    paste("Max Degree:", results()$max_degree)
  })
  
  output$diameter <- renderText({
    req(results())
    paste("Graph Diameter:", results()$diameter)
  })
  
  output$clustering <- renderText({
    req(results())
    paste("Average Clustering Coefficient:", round(results()$avg_clustering, 3))
  })
  
  # Enable download of metrics
  output$downloadMetrics <- downloadHandler(
    filename = function() { "graph_metrics.csv" },
    content = function(file) {
      req(results())
      write.csv(results()$metrics, file, row.names = FALSE)
    }
  )
  
  # Output adjacency matrix
  output$adjMatrix <- renderTable({
    req(results())
    results()$adjacency_matrix
  }, rownames = TRUE)
  
  
  # Network visualization
  output$network <- renderVisNetwork({
    req(results())
    g <- results()$graph
    
    # Prepare nodes for visNetwork
    nodes <- data.frame(id = V(g)$name, label = V(g)$name)
    
    # Prepare edges for visNetwork
    edges_vis <- data.frame(from = as.character(ends(g, E(g))[, 1]),
                            to = as.character(ends(g, E(g))[, 2]))
    
    # Create the visNetwork plot
    visNetwork(nodes, edges_vis) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  # Save network as an image
  output$downloadNetwork <- downloadHandler(
    filename = function() { "network_visualization.png" },
    content = function(file) {
      req(results())
      g <- results()$graph
      
      # Prepare the visNetwork plot
      vis_net <- visNetwork(nodes = data.frame(id = V(g)$name, label = V(g)$name),
                            edges = data.frame(from = as.character(ends(g, E(g))[, 1]),
                                               to = as.character(ends(g, E(g))[, 2]))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visEdges(smooth = TRUE) %>%
        visLayout(randomSeed = 42)
      
      # Save the plot as an HTML file first
      temp_html <- tempfile(fileext = ".html")
      visSave(vis_net, temp_html)
      
      # Capture and save the HTML file as a PNG image using webshot
      webshot::webshot(temp_html, file = file, vwidth = 800, vheight = 600)
    }
  )
  
  output$degreeDistPlot <- renderPlotly({
    req(results())
    g <- results()$graph
    
    # Get the degrees of the nodes
    degrees <- degree(g, mode = "all", normalized = input$normalized)
    
    # Create a data frame with degrees
    degree_data <- data.frame(degree = degrees)
    
    # Count the occurrences of each degree
    degree_counts <- degree_data %>%
      group_by(degree) %>%
      summarise(count = n(), .groups = 'drop')
    
    # Calculate the probability by dividing the count by the total number of nodes
    total_nodes <- length(degrees)
    degree_counts$probability <- degree_counts$count / total_nodes
    
    # Create the ggplot for the degree distribution with probability
    ggplot(degree_counts, aes(x = degree, y = probability)) +
      geom_bar(stat = "identity", fill = "purple", color = "black", width = 0.5) +
      labs(title = "Degree Distribution (Probability)", x = "Degree", y = "Probability") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min(degree_counts$degree), max(degree_counts$degree), 1)) +
      scale_y_continuous(labels = scales::percent) # To format the y-axis as percentages
  })

}

# Run the app
shinyApp(ui = ui, server = server)

