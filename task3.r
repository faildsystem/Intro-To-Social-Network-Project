# Required packages

packages <-
  c(
    "igraph",
    "tidyverse",
    "ggplot2",
    "reshape2",
    "rgl",
    "plotly",
    "ff",
    "dplyr"
    
  )

# Loop to install packages if they are not already installed

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    
    # Load the package after installation
    
    library(package, character.only = TRUE)
  }
}


# Function to read the edges from the CSV files

read_edges <- function(path) {
  edges <- read.csv(path)
  return(edges)
}

# Read edges from all data files

public <- read_edges("facebook_clean_data/public_figure_edges.csv")
tv <- read_edges("facebook_clean_data/tvshow_edges.csv")
pol <- read_edges("facebook_clean_data/politician_edges.csv")
artist <- read_edges("facebook_clean_data/artist_edges.csv")
athlete <- read_edges("facebook_clean_data/athletes_edges.csv")
company <- read_edges("facebook_clean_data/company_edges.csv")
site <- read_edges("facebook_clean_data/new_sites_edges.csv")
gov <- read_edges("facebook_clean_data/government_edges.csv")

# Combine all datasets

all_edges <- bind_rows(
  public,
  tv,
  pol,
  artist,
  athlete,
  company,
  site,
  gov
)


# Print the first few rows to verify

head(all_edges)


# Export the merged dataset to a CSV file

write.csv(all_edges, "merged_edges.csv", row.names = FALSE)


edges_df <- data.frame(all_edges)

function(graph, node_index) {
  if (!(node_index %in% V(graph)$name)) {
    stop("Node index not found in the graph!")
  }
  
  # Existing Metrics
  neighbors <- neighbors(graph, node_index, mode = "all")
  cf <- transitivity(graph, vids = node_index, type = "local")
  closeness <- closeness(graph, vids = node_index, normalized = TRUE)
  betweenness_value <- betweenness(graph, v = node_index, directed = FALSE, normalized = TRUE)
  degree_centrality <- degree(graph, v = node_index, normalized = TRUE)
  degree_distribution <- degree_distribution(graph)
  eccentricity_value <- eccentricity(graph, vids = node_index)
  
  # New Metrics
  # Community Detection
  community_det <- cluster_walktrap(graph)
  membership <- membership(community_det)[node_index]
  
  # Connectivity
  graph_connectivity <- vertex_connectivity(graph)
  node_connectivity <- vertex_connectivity(graph, source = node_index)
  
  # PageRank
  page_rank_value <- page_rank(graph)$vector[node_index]
  
  # Eigenvector Centrality
  eigenvector_centrality <- eigen_centrality(graph)$vector[node_index]
  
  # Comprehensive Results
  result <- list(
    Neighbors = as.numeric(neighbors),
    Clustering_Coefficient = cf,
    Closeness = closeness,
    Betweenness = betweenness_value,
    Degree_Centrality = degree_centrality,
    Degree_Distribution = degree_distribution,
    Eccentricity = eccentricity_value,
    Community_Membership = membership,
    Graph_Connectivity = graph_connectivity,
    Node_Connectivity = node_connectivity,
    PageRank = page_rank_value,
    Eigenvector_Centrality = eigenvector_centrality
  )
  
  return(result)
}
