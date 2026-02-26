library(dplyr)
library(readr)
library(ggplot2)
library(purrr)
library(tidyr)

data_dir <- "data"

#======================================
# Load grid data
# PA: 1 = protected, 0 = unprotected
#======================================
grid_df <- read_csv(file.path(data_dir, "grid_details.csv")) %>%
  select(Gridcode, PA, Area)

#======================================
# List species-per-grid files
#======================================

species_files <- list.files(
  data_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

#======================================
# Function to compute partial coverage 
#======================================

compute_partial_coverage <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file) %>%
    left_join(grid_df, by = "Gridcode")
  
  coverage_df <- spp_df %>%
    group_by(species) %>%
    summarise(
      total_area = sum(Area, na.rm = TRUE),
      protected_area = sum(Area[PA == 1], na.rm = TRUE),
      coverage_percent = 100 * protected_area / total_area,
      .groups = "drop"
    ) %>%
    mutate(taxon = taxon)
  
  return(coverage_df)
}

#======================================
# Compute coverage for all taxa
#======================================
coverage_results <- map_df(species_files, compute_partial_coverage)

# Save results
write_csv(
  coverage_results,
  file.path(data_dir, "species_partial_coverage.csv")
)