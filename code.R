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

data_path <- "facebook_clean_data/"

public <- read_edges(paste0(data_path, "public_figure_edges.csv"))
tv <- read_edges(paste0(data_path, "tvshow_edges.csv"))
pol <- read_edges(paste0(data_path, "politician_edges.csv"))
artist <- read_edges(paste0(data_path, "artist_edges.csv"))
athlete <- read_edges(paste0(data_path, "athletes_edges.csv"))
company <- read_edges(paste0(data_path, "company_edges.csv"))
site <- read_edges(paste0(data_path, "new_sites_edges.csv"))
gov <- read_edges(paste0(data_path, "government_edges.csv"))

# Combine all datasets

all_edges <- bind_rows(
  public = public,
  tv = tv,
  politician = pol,
  artist = artist,
  athlete = athlete,
  company = company,
  news_site = site,
  government = gov,
  .id = "source"
)

# Print the first few rows to verify

head(all_edges)


# Export the merged dataset to a CSV file

write.csv(all_edges, "merged_edges.csv", row.names = FALSE)





