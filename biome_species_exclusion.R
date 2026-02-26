# ===============================
# Load libraries and read data
# ===============================
library(sf)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(ggplot2)

grid_file   <- "10km_grid.shp"
biome_file  <- "Nepal_biome.shp"
pa_file     <- "Nepal_PA_Behrmann.shp"
species_dir <- "data"

grid_sf  <- st_read(grid_file, quiet = TRUE) %>% st_transform(6933)
biome_sf <- st_read(biome_file, quiet = TRUE) %>% st_transform(6933)
pa_sf    <- st_read(pa_file, quiet = TRUE) %>% st_transform(6933)

# ===============================
# Add biome info to grid
# ===============================
grid_biome <- st_join(grid_sf, biome_sf["BIOME_NAME"]) %>%
  st_drop_geometry() %>%
  select(Gridcode, BIOME_NAME)

# Add PA info to grid
grid_pa <- st_join(grid_sf, pa_sf["NAME_ENG"]) %>%
  st_drop_geometry() %>%
  select(Gridcode, PA = NAME_ENG) %>%
  mutate(PA = ifelse(is.na(PA), 0, 1))  # PA = 1 if protected, 0 otherwise

# Combine grid info
grid_info <- grid_biome %>%
  left_join(grid_pa, by = "Gridcode")

# ===============================
# List species files
# ===============================
species_files <- list.files(
  species_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

# ==============================================
# Compute % species excluded per biome × taxon
# ==============================================
compute_exclusion <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file, show_col_types = FALSE) %>%
    left_join(grid_info, by = "Gridcode")
  
  # Determine if species occurs in PA
  species_status <- spp_df %>%
    group_by(species, BIOME_NAME) %>%
    summarise(
      protected_anywhere = any(PA == 1),
      .groups = "drop"
    )
  
  # Summarise % excluded per biome
  summary_df <- species_status %>%
    group_by(BIOME_NAME) %>%
    summarise(
      total_species = n(),
      excluded_species = sum(!protected_anywhere),
      percent_excluded = 100 * excluded_species / total_species,
      .groups = "drop"
    ) %>%
    mutate(taxon = taxon)
  
  return(summary_df)
}

exclusion_df <- map_df(species_files, compute_exclusion)

# ===============================
# Merge plant groups
# ===============================

exclusion_df <- exclusion_df %>%
  mutate(
    taxon = ifelse(taxon %in% c("liliopsida", "magnoliopsida"), "Plants", taxon)
  ) %>%
  group_by(BIOME_NAME, taxon) %>%
  summarise(
    total_species = sum(total_species),
    excluded_species = sum(excluded_species),
    percent_excluded = 100 * excluded_species / total_species,
    .groups = "drop"
  )

# Reorder taxa
exclusion_df$taxon <- factor(exclusion_df$taxon,
                             levels = c("amphibian", "reptile", "aves", "mammal", "Plants"))

# ====================================
# Heatmap with multi-color gradient
# ====================================

ggplot(exclusion_df, aes(x = taxon, y = BIOME_NAME, fill = percent_excluded)) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  # Multi-color gradient: 5 colors
  scale_fill_gradientn(
    colors = c("#4575b4", "#ffffbf",  "#d73027", "#7f2704"),  
    values = scales::rescale(c(0, 20, 30, 60)),  # map % excluded to gradient stops
    limits = c(0, 60),
    name = "% species excluded"
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    title = " "
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
