# Load libraries
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)


data_dir <- "data"

#======================================
# Read grid protection data
#======================================
grid_df <- read_csv(file.path(data_dir, "grid_details.csv")) %>%
  select(Gridcode, PA)

#======================================
# List species-per-grid files
#======================================

species_files <- list.files(
  path = data_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

#======================================
# Function to compute species exclusion
#======================================
compute_species_exclusion <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file) %>%
    left_join(grid_df, by = "Gridcode")
  
  species_status <- spp_df %>%
    group_by(species) %>%
    summarise(
      protected_anywhere = any(PA == 1),
      .groups = "drop"
    )
  
  summary <- species_status %>%
    summarise(
      taxon = taxon,
      total_species = n(),
      protected_species = sum(protected_anywhere),
      excluded_species = sum(!protected_anywhere),
      protected_percent = 100 * protected_species / total_species,
      excluded_percent = 100 * excluded_species / total_species
    )
  
  return(summary)
}

#==========================
# Run for all taxa
#==========================
exclusion_results <- map_df(species_files, compute_species_exclusion)

# ---- Merge liliopsida and magnoliopsida into Plants ----
exclusion_results <- exclusion_results %>%
  mutate(taxon = ifelse(taxon %in% c("liliopsida", "magnoliopsida"), "Plants", taxon)) %>%
  group_by(taxon) %>%
  summarise(
    total_species = sum(total_species),
    protected_species = sum(protected_species),
    excluded_species = sum(excluded_species),
    protected_percent = 100 * protected_species / total_species,
    excluded_percent = 100 * excluded_species / total_species,
    .groups = "drop"
  )

#=====================
#Reorder taxa
#=====================
exclusion_results <- exclusion_results %>%
  mutate(
    taxon = factor(
      taxon,
      levels = c("Plants", "mammal", "aves", "reptile", "amphibian")
    )
  )


#=========================================
#  Define a color palette
#=========================================
taxon_colors <- c(
  "amphibian" = "#009E73",  # bluish green
  "reptile"   = "#D55E00",  # vermillion
  "mammal"    = "#0072B2",  # blue
  "aves"      = "#E69F00",  # orange
  "Plants"    = "#CC79A7"   # reddish purple
)


#======================================
# Plot species exclusion
#======================================

ggplot(exclusion_results,
       aes(x = taxon, y = excluded_percent, fill = taxon)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = taxon_colors, guide = "none") +  # <- replace Viridis
  labs(
    x = NULL,
    y = "Species excluded by Nepal's PA network (%)",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
