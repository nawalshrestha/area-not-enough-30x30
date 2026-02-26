# ============================================================
# Species richness per biome by taxonomic group
# ============================================================

library(sf)
library(dplyr)
library(purrr)
library(ggplot2)


grid_file   <- "10km_grid.shp"
biome_file  <- "Nepal_biome.shp"
species_dir <- "data"

#======================================
# Read spatial data
#======================================
grid_sf  <- st_read(grid_file, quiet = TRUE) |> st_transform(6933)
biome_sf <- st_read(biome_file, quiet = TRUE) |> st_transform(6933)

#======================================
# Assign biome to grid
#======================================
grid_biome <- st_join(grid_sf, biome_sf["BIOME_NAME"], left = TRUE) |>
  st_drop_geometry() |>
  select(Gridcode, BIOME_NAME)

#======================================
# Read species files
#======================================
species_files <- list.files(
  species_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

#======================================
# Compute species richness per biome
#======================================
richness_fun <- function(file) {
  
  taxon_raw <- gsub("_species_per_grid.csv", "", basename(file))
  
  spp_df <- read_csv(file, show_col_types = FALSE) |>
    left_join(grid_biome, by = "Gridcode") |>
    filter(!is.na(BIOME_NAME)) |>
    distinct(species, BIOME_NAME)
  
  spp_df |>
    count(BIOME_NAME, name = "richness") |>
    mutate(taxon_raw = taxon_raw)
}

rich_df <- map_df(species_files, richness_fun)

#======================================
# Merge plant groups
#======================================
rich_df <- rich_df |>
  mutate(
    taxon = case_when(
      taxon_raw %in% c("magnoliopsida", "liliopsida") ~ "Plants",
      taxon_raw == "amphibian" ~ "Amphibians",
      taxon_raw == "reptile"   ~ "Reptiles",
      taxon_raw == "aves"      ~ "Birds",
      taxon_raw == "mammal"    ~ "Mammals",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(taxon))

#======================================
# Sum richness for merged Plants
#======================================
rich_sum <- rich_df |>
  group_by(taxon, BIOME_NAME) |>
  summarise(richness = sum(richness), .groups = "drop")

#======================================
# Order taxa and biomes
#======================================
rich_sum$taxon <- factor(
  rich_sum$taxon,
  levels = c("Amphibians", "Reptiles", "Birds", "Mammals", "Plants")
)

#======================================
# Plot: faceted barplots
#======================================
ggplot(rich_sum,
       aes(x = BIOME_NAME, y = richness)) +
  geom_col(fill = "#1b9e77", width = 0.8) +
  coord_flip() +
  facet_wrap(
    ~ taxon,
    ncol = 2,        # 2 columns
    nrow = 3,        # 3 rows
    scales = "free_x"
  ) +
  labs(
    x = NULL,
    y = "Species richness",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.22, "cm"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black")
  )
