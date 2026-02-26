# ===============================
# Load libraries
# ===============================
library(sf)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)

set.seed(123)  # For reproducibility

grid_file   <- "10km_grid.shp"
biome_file  <- "Nepal_biome.shp"
pa_file     <- "Nepal_PA_Behrmann.shp"
species_dir <- "data"

# ===============================
# Read spatial layers
# ===============================
grid_sf  <- st_read(grid_file, quiet = TRUE) %>% st_transform(6933)
biome_sf <- st_read(biome_file, quiet = TRUE) %>% st_transform(6933)
pa_sf    <- st_read(pa_file, quiet = TRUE) %>% st_transform(6933)

# ===============================
# Assign biome to grid
# ===============================
grid_biome <- st_join(grid_sf, biome_sf["BIOME_NAME"]) %>%
  st_drop_geometry() %>%
  select(Gridcode, BIOME_NAME)

# ===============================
# Assign PA status to grid
# ===============================
grid_pa <- st_join(grid_sf, pa_sf["NAME_ENG"]) %>%
  st_drop_geometry() %>%
  select(Gridcode, PA = NAME_ENG) %>%
  mutate(PA = ifelse(is.na(PA), 0, 1))

# ===============================
# Final grid info
# ===============================
grid_info <- grid_biome %>%
  left_join(grid_pa, by = "Gridcode")

# ===============================
# Species files
# ===============================
species_files <- list.files(
  species_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

# ======================================
# Function: observed species protection
# ======================================
compute_observed <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file, show_col_types = FALSE) %>%
    left_join(grid_info, by = "Gridcode")
  
  spp_status <- spp_df %>%
    group_by(species) %>%
    summarise(protected = any(PA == 1), .groups = "drop")
  
  tibble(
    taxon = taxon,
    total_species = nrow(spp_status),
    protected_species = sum(spp_status$protected),
    prop_protected = protected_species / total_species
  )
}

observed_df <- map_df(species_files, compute_observed)

# ====================================
# Null model: Randomize PA grid cells
# ====================================
n_iter <- 999

run_null <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file, show_col_types = FALSE) %>%
    left_join(grid_info, by = "Gridcode")
  
  pa_cells <- grid_info %>% filter(PA == 1) %>% pull(Gridcode)
  n_pa     <- length(pa_cells)
  all_cells <- grid_info$Gridcode
  
  null_props <- replicate(n_iter, {
    
    random_pa <- sample(all_cells, n_pa)
    
    spp_status <- spp_df %>%
      mutate(PA_rand = ifelse(Gridcode %in% random_pa, 1, 0)) %>%
      group_by(species) %>%
      summarise(protected = any(PA_rand == 1), .groups = "drop")
    
    mean(spp_status$protected)
  })
  
  tibble(
    taxon = taxon,
    null_mean = mean(null_props),
    null_sd   = sd(null_props),
    null_q05  = quantile(null_props, 0.05),
    null_q95  = quantile(null_props, 0.95)
  )
}

null_df <- map_df(species_files, run_null)

# ===============================
# Combine observed vs null
# ===============================
comparison_df <- observed_df %>%
  left_join(null_df, by = "taxon") %>%
  mutate(
    z_score = (prop_protected - null_mean) / null_sd,
    below_random = prop_protected < null_q05
  )

# ===============================
# Final plot
# ===============================

comparison_df2 <- comparison_df %>%
  mutate(
    taxon = case_when(
      taxon %in% c("magnoliopsida", "liliopsida") ~ "plants",
      TRUE ~ taxon
    )
  ) %>%
  group_by(taxon) %>%
  summarise(
    prop_protected = mean(prop_protected),
    null_mean      = mean(null_mean),
    null_sd        = mean(null_sd),
    z_score        = (prop_protected - null_mean) / null_sd,
    .groups = "drop"
  )

comparison_df2$taxon <- factor(
  comparison_df2$taxon,
  levels = c("amphibian", "reptile", "aves", "mammal", "plants")
)


ggplot(comparison_df2,
       aes(x = taxon, y = z_score)) +
  geom_col(fill = "#4575b4", width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.9) +
  labs(
    x = NULL,
    y = "Z-score (observed − random)",
    title = "Protected area performance relative to random expectation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # bounding box
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.22, "cm"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black")
  )