# ===============================
# Load libraries
# ===============================
library(sf)
library(terra)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)

grid_file   <- "10km_grid.shp"
elev_file   <- "nepal_srtm.tif"
species_dir <- "data"

# ===============================
# Read spatial data
# ===============================
grid_sf <- st_read(grid_file, quiet = TRUE) %>% st_transform(6933)
elev_r  <- rast(elev_file)

# ===============================
# Extract mean elevation per grid
# ===============================
grid_sf$elev_mean <- terra::extract(
  elev_r,
  vect(grid_sf),
  fun = mean,
  na.rm = TRUE
)[, 2]

# ===============================
# Create custom elevation bands
# ===============================
grid_elev <- grid_sf %>%
  mutate(
    elev_band = cut(
      elev_mean,
      breaks = c(-Inf, 1000, 2000, 3000, 4000, 5000, 6000, Inf),
      labels = c("<1000", "1000–2000", "2000–3000", "3000–4000",
                 "4000–5000", "5000–6000", ">6000"),
      right = FALSE
    )
  ) %>%
  st_drop_geometry() %>%
  select(Gridcode, elev_band)

# ===============================
# List species files
# ===============================
species_files <- list.files(
  species_dir,
  pattern = "_species_per_grid.csv$",
  full.names = TRUE
)

# ===================================================
# Count threatened species per taxon × elevation band
# ===================================================
count_threatened <- function(file) {
  
  taxon <- gsub("_species_per_grid.csv", "", basename(file))
  
  read_csv(file, show_col_types = FALSE) %>%
    left_join(grid_elev, by = "Gridcode") %>%
    distinct(species, elev_band) %>%
    count(elev_band, name = "n_threatened") %>%
    mutate(taxon = taxon)
}

threat_df <- map_df(species_files, count_threatened)

# ===============================
# Merge plant groups
# ===============================
threat_df <- threat_df %>%
  mutate(
    taxon = case_when(
      taxon %in% c("magnoliopsida", "liliopsida") ~ "plants",
      TRUE ~ taxon
    )
  ) %>%
  group_by(taxon, elev_band) %>%
  summarise(n_threatened = sum(n_threatened), .groups = "drop")

# ====================================
# Order factors
# ====================================
threat_df$taxon <- factor(
  threat_df$taxon,
  levels = c("amphibian", "reptile", "aves", "mammal", "plants")
)

threat_df$elev_band <- factor(
  threat_df$elev_band,
  levels = c("<1000", "1000–2000", "2000–3000", "3000–4000",
             "4000–5000", "5000–6000", ">6000")
)

# ===============================
# Plot figure
# ===============================
ggplot(threat_df, aes(x = elev_band, y = n_threatened)) +
  geom_col(fill = "#1b9e77") +
  facet_wrap(~ taxon, ncol = 2, scales = "free_x") +
  coord_flip() +
  labs(
    x = "Elevation band (m)",
    y = "Number of threatened species",
    title = " "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    #panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # bounding box
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.22, "cm"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black")
  )