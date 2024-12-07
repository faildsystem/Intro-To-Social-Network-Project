# Load necessary libraries
# install.packages("reshape2", dependencies = TRUE)
library(dplyr)
library(igraph)
library(ggplot2)
library(reshape2)

# Load the CSV file into a data frame
file_path <- "merged_edges.csv"
df <- read.csv(file_path, header = TRUE)

# View the structure of the data frame
str(df)

# Create a graph from the edge list
graph <- graph_from_data_frame(df, directed = FALSE)

# Function to calculate metrics for a given node index
calculate_metrics <- function(node_index, graph) {
  # Convert to character to match vertex names
  node_index <- as.character(node_index)
  print(node_index)
  
  # Check if the node exists in the graph
  if (!(node_index %in% V(graph)$name)) {
    stop("Node index not found in the graph!")
  }
  
  # Neighbors
  neighbors <- neighbors(graph, node_index, mode = "all")
  
  # Clustering Coefficient
  cf <- transitivity(graph, vids = node_index, type = "local")
  
  # Closeness Centrality
  closeness <- closeness(graph, vids = node_index, normalized = TRUE)
  
  # Betweenness Centrality
  # betweenness_value <- betweenness(graph, v = node_index, directed = FALSE, normalized = TRUE)
  
  # Degree Centrality
  degree_centrality <- degree(graph, v = node_index, normalized = TRUE)
  
  # Degree Distribution
  degree_distribution <- degree_distribution(graph)
  
  # Additional Metric: Eccentricity
  eccentricity_value <- eccentricity(graph, vids = node_index)
  
  # Format results
  result <- list(
    Neighbors = as.numeric(neighbors),
    Clustering_Coefficient = cf,
    Closeness = closeness,
    # Betweenness = betweenness_value,
    Degree_Centrality = degree_centrality,
    Degree_Distribution = degree_distribution,
    Eccentricity = eccentricity_value
  )
  
  return(result)
}

# Example usage
node_index <- 1  # Replace this with the node index provided by the user
metrics <- calculate_metrics(node_index, graph)
print(metrics)





# 
# # Metrics Data
# metrics_data <- data.frame(
#   Metric = c("Clustering Coefficient", "Closeness", "Degree Centrality", "Eccentricity"), # add betweeness
#   Value = c(
#     metrics$Clustering_Coefficient,
#     metrics$Closeness,
#     # metrics$Betweenness,
#     metrics$Degree_Centrality,
#     metrics$Eccentricity
#   )
# )

# # 1. Bar Plot for Clustering Coefficient
# ggplot(metrics_data[1, , drop = FALSE], aes(x = Metric, y = Value, fill = Metric)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Clustering Coefficient", y = "Value", x = NULL) +
#   theme_minimal() +
#   scale_fill_brewer(palette = "Set2") +
#   guides(fill = "none")
# 
# # 2. Scatter Plot for Closeness Centrality
# ggplot(data.frame(Node = node_index, Closeness = metrics$Closeness), aes(x = Node, y = Closeness)) +
#   geom_point(color = "blue", size = 3) +
#   labs(title = "Closeness Centrality", y = "Closeness", x = "Node") +
#   theme_minimal()
# 
# # # 3. Line Plot for Betweenness Centrality
# # ggplot(data.frame(Node = node_index, Betweenness = metrics$Betweenness), aes(x = Node, y = Betweenness)) +
# #   geom_line(color = "green", size = 1.2) +
# #   labs(title = "Betweenness Centrality", y = "Betweenness", x = "Node") +
# #   theme_minimal()
# 
# # 4. Bar Plot for Degree Centrality
# ggplot(data.frame(Node = node_index, Degree = metrics$Degree_Centrality), aes(x = Node, y = Degree, fill = "Degree Centrality")) +
#   geom_bar(stat = "identity") +
#   labs(title = "Degree Centrality", y = "Centrality", x = "Node") +
#   theme_minimal() +
#   guides(fill = "none")
# 
# # 5. Line Plot for Eccentricity
# ggplot(data.frame(Node = node_index, Eccentricity = metrics$Eccentricity), aes(x = Node, y = Eccentricity)) +
#   geom_line(color = "red", size = 1.2) +
#   labs(title = "Eccentricity", y = "Eccentricity", x = "Node") +
#   theme_minimal()
# 
# # 6. Histogram for Degree Distribution
# degree_distribution_data <- data.frame(
#   Degree = seq_along(metrics$Degree_Distribution),
#   Frequency = metrics$Degree_Distribution
# )
# 
# ggplot(degree_distribution_data, aes(x = Degree, y = Frequency)) +
#   geom_histogram(stat = "identity", fill = "purple", color = "black", binwidth = 1) +
#   labs(title = "Degree Distribution", x = "Degree", y = "Frequency") +
#   theme_minimal()

