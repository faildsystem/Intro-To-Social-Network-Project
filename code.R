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
  public = public,
  tv = tv,
  politician = pol,
  artist = artist,
  athlete = athlete,
  company = company,
  news_site = site,
  government = gov,
)

# Print the first few rows to verify

head(all_edges)


# Export the merged dataset to a CSV file

write.csv(all_edges, "merged_edges.csv", row.names = FALSE)