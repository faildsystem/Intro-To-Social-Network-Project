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


# Function to calculate graph metrics

calculate_metrics <- function(edges) {
  # Create graph object (undirected)
  g <- graph_from_data_frame(edges, directed = FALSE)
  
  # Calculate metrics
  metrics <- data.frame(
    node = V(g)$name,
    degree_centrality = degree(g, mode = "all"),
    betweenness_centrality = betweenness(g, directed = FALSE, normalized = TRUE),
    closeness_centrality = closeness(g, normalized = TRUE),
    clustering_coefficient = transitivity(g, type = "local", isolates = "zero")
  )
  
  # Round metrics to 3 decimal places
  metrics <- metrics %>%
    mutate(
      degree_centrality = round(degree_centrality, 3),
      betweenness_centrality = round(betweenness_centrality, 3),
      closeness_centrality = round(closeness_centrality, 3),
      clustering_coefficient = round(clustering_coefficient, 3)
    )
  
  # Calculate the longest shortest path (graph diameter)
  graph_diameter <- diameter(g, directed = FALSE, weights = NA)

  avg_clustering_coef <- transitivity(g, type = "average")
  
  list(metrics = metrics, longest_shortest_path = graph_diameter, avg_clustering_coef = avg_clustering_coef)
}

results <- calculate_metrics(edges_df[1:1000, ])

# Extract metrics and longest shortest path
graph_metrics <- results$metrics
avg_clustering_coef <- results$avg_clustering_coef
longest_shortest_path <- results$longest_shortest_path

# Print the first few rows of the metrics
head(graph_metrics)


print(paste("Longest Shortest Path (Diameter):", longest_shortest_path))

print(paste("Average Clustering Coefficient:", round(avg_clustering_coef, 3)))

# Export metrics to a CSV file
write.csv(graph_metrics, "graph_metrics.csv", row.names = FALSE)


