# # List of required packages
# packages <- c(
#   "shiny",
#   "shinycssloaders",
#   "igraph",
#   "tidyverse",
#   "ggplot2",
#   "reshape2",
#   "rgl",
#   "plotly",
#   "ff",
#   "dplyr",
#   "visNetwork",
#   "webshot",
#   "linkcomm"
# )
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
# webshot::install_phantomjs()


# Increase file upload size to 50 MB
options(shiny.maxRequestSize = 50 * 1024^2)

# UI definition
ui <- fluidPage(
  titlePanel("Advanced Network Analysis Tool"),
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
                 withSpinner(textOutput("clustering"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Network Visualization", 
                 downloadButton("downloadNetwork", "Download Network Image", style = "margin: 15px 0;"),
                 withSpinner(visNetworkOutput("network"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Centrality Metrics",
                 downloadButton("downloadCentrality", "Download Centrality Metrics", style = "margin: 15px 0;"),
                 withSpinner(tableOutput("centralityMetrics"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Adjacency Matrix",
                 br(),
                 withSpinner(tableOutput("adjMatrix"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Degree Distribution",
                 withSpinner(plotlyOutput("degreeDistPlot"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Community Detection",
                 downloadButton("downloadCommunities", "Download Community Metrics", style = "margin: 15px 0;"),
                 withSpinner(tableOutput("communityDetectionTable"), type = 7, color = "#0d6efd"),
                 withSpinner(plotlyOutput("communityPlot"), type = 7, color = "#0d6efd")
        ),
        tabPanel("Path Analysis",
                 uiOutput("nodeSelectorPath"),
                 actionButton("calculatePath", "Calculate Path Between Nodes"),
                 textOutput("pathResult"),
                 tableOutput("pathDetails")
        ),
        tabPanel("Node Metrics",
                 uiOutput("nodeSelector"),
                 actionButton("calculateNodeMetrics", "Calculate Node Metrics"),
                 tableOutput("nodeMetricsTable")
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
    
    # Enhanced metrics calculation function
    calculate_metrics <- function(edges, directed, isNormalized) {
      # Create graph
      g <- graph_from_data_frame(edges, directed = (directed == "directed"))
      
      # Centrality Metrics
      centrality_metrics <- data.frame(
        node = V(g)$name,
        degree_centrality = degree(g, mode = "all", normalized = isNormalized),
        betweenness_centrality = betweenness(g, directed = (directed == "directed"), normalized = isNormalized),
        closeness_centrality = closeness(g, normalized = isNormalized),
        eigenvector_centrality = eigen_centrality(g)$vector,
        page_rank = page_rank(g)$vector,
        local_clustering_coefficient = transitivity(g, type = "local", isolates = "zero")
      ) %>% 
        mutate(across(where(is.numeric), ~round(., 4)))
      
      # Adjacency Matrix
      adjacency <- as.matrix(as_adjacency_matrix(g, sparse = FALSE))
      
      # Community Detection
      lc <- linkcomm::getLinkCommunities(as.data.frame(ends(g, E(g))))
      
      # Calculate community metrics
      community_metrics <- data.frame(
        community_id = seq_along(lc$numclusters),
        num_nodes = sapply(lc$clusters, length),
        num_edges = sapply(lc$clusters, function(x) length(unique(unlist(x))))
      )
      
      # Additional graph metrics
      list(
        graph = g,
        centrality_metrics = centrality_metrics,
        diameter = diameter(g, directed = (directed == "directed"), weights = NA),
        avg_clustering = transitivity(g, type = "average"),
        min_degree = min(degree(g, mode = "all", normalized = isNormalized)),
        max_degree = max(degree(g, mode = "all", normalized = isNormalized)),
        avg_degree = round(mean(degree(g, mode = "all", normalized = isNormalized)), 3),
        connected = is_connected(g),
        nodes = vcount(g),
        edges = ecount(g),
        adjacency_matrix = adjacency,
        community_metrics = community_metrics,
        link_communities = lc
      )
    }
    
    # Perform calculations
    results(calculate_metrics(edges(), input$graphType, input$normalized))
    
    #  loading message
    removeModal()
    
    # Update node selector for path analysis
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
    
    # Calculate path between two nodes
    observeEvent(input$calculatePath, {
      req(input$node1, input$node2)
      g <- results()$graph
      
      # Find shortest path
      firstNodeIndex <- match(input$node1, V(g)$name)
      secondNodeIndex <- match(input$node2, V(g)$name)
      
      path <- shortest_paths(g, from = firstNodeIndex, to = secondNodeIndex)
      
      # Prepare path details
      if (length(path$vpath[[1]]) > 0) {
        path_nodes <- V(g)$name[path$vpath[[1]]]
        path_details <- data.frame(
          node = path_nodes,
          degree = degree(g, v = path$vpath[[1]], mode = "all"),
          betweenness = betweenness(g, v = path$vpath[[1]], normalized = input$normalized)
        )
        
        output$pathResult <- renderText({
          paste("Path:", paste(path_nodes, collapse = " >>> "))
        })
        
        output$pathDetails <- renderTable({
          path_details
        })
      } else {
        output$pathResult <- renderText({
          "No path found between the selected nodes"
        })
        
        output$pathDetails <- renderTable({
          data.frame(Message = "No path details available")
        })
      }
    })
  })
  
  # Centrality Metrics Table
  output$centralityMetrics <- renderTable({
    req(results())
    results()$centrality_metrics
  })
  
  # Download Centrality Metrics
  output$downloadCentrality <- downloadHandler(
    filename = function() { "centrality_metrics.csv" },
    content = function(file) {
      req(results())
      write.csv(results()$centrality_metrics, file, row.names = FALSE)
    }
  )
  
  # Community Detection Table
  output$communityDetectionTable <- renderTable({
    req(results())
    results()$community_metrics
  })
  
  # Community Detection Plot
  output$communityPlot <- renderPlotly({
    req(results())
    community_metrics <- results()$community_metrics
    
    # Create a bar plot of community sizes
    plot_ly(community_metrics, x = ~community_id, y = ~num_nodes, 
            type = 'bar', name = 'Nodes',
            marker = list(color = 'blue', opacity = 0.7)) %>%
      add_trace(y = ~num_edges, name = 'Edges', 
                marker = list(color = 'red', opacity = 0.7)) %>%
      layout(title = 'Community Size Distribution',
             xaxis = list(title = 'Community ID'),
             yaxis = list(title = 'Count'),
             barmode = 'group')
  })
  
  # Download Community Metrics
  output$downloadCommunities <- downloadHandler(
    filename = function() { "community_metrics.csv" },
    content = function(file) {
      req(results())
      write.csv(results()$community_metrics, file, row.names = FALSE)
    }
  )
  
  # Function for calculating metrics for a specific node
  calculate_node_metrics <- function(node_name) {
    # Get the graph from results
    g <- results()$graph
    
    # Find the node index from node name
    node <- match(node_name, V(g)$name)

    node_metrics <- results()$centrality_metrics[node, ]
    
    # Get the neighbors of the node
    neighbors <- neighbors(g, node)
    
    # Retrieve the names of the neighbors
    neighbor_names <- V(g)[neighbors]$name
    
    node_metrics$neighbors <- paste(neighbor_names, collapse = ", ")
    
    return(node_metrics)
  }
  
  
  # Calculate node metrics when button is clicked
  observeEvent(input$calculateNodeMetrics, {
    req(input$nodeIndex)
    node_metrics <- calculate_node_metrics(input$nodeIndex)
    
    output$nodeMetricsTable <- renderTable({
      
      node_metrics
      
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
    nodes <- data.frame(
      id = V(g)$name, 
      label = V(g)$name,
      color = results()$centrality_metrics$page_rank  # Color nodes based on PageRank
    )
    
    # Prepare edges for visNetwork
    edges_vis <- data.frame(
      from = as.character(ends(g, E(g))[, 1]),
      to = as.character(ends(g, E(g))[, 2])
    )
    
    # Create the visNetwork plot
    visNetwork(nodes, edges_vis) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEdges(smooth = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
   
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