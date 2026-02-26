# ===============================
# Load libraries
# ===============================
library(sf)
library(dplyr)
library(units)
library(ggplot2)


biome_file <- "Nepal_biome.shp"
pa_file    <- "Nepal_PA_Behrmann.shp"
grid_file  <- "10km_grid.shp"


# ===============================
# Read spatial layers
# ===============================
biome_sf <- st_read(biome_file, quiet = TRUE) %>%
  st_transform(6933)      # Behrmann equal-area

pa_sf <- st_read(pa_file, quiet = TRUE) %>%
  st_transform(6933)

grid_sf <- st_read(grid_file, quiet = TRUE) %>%
  st_transform(6933)

# ===============================
#  BIOME AREA & PROTECTED AREA
# ===============================

# Total biome area
biome_area <- biome_sf %>%
  mutate(area_km2 = set_units(st_area(.), "km^2")) %>%
  st_drop_geometry() %>%                         
  group_by(BIOME_NAME) %>%
  summarise(
    total_area = sum(area_km2),
    .groups = "drop"
  )

# Protected biome area
biome_pa_area <- st_intersection(biome_sf, pa_sf) %>%
  mutate(area_km2 = set_units(st_area(.), "km^2")) %>%
  st_drop_geometry() %>%                         
  group_by(BIOME_NAME) %>%
  summarise(
    protected_area = sum(area_km2),
    .groups = "drop"
  )

# Join + compute percent protected
biome_area_df <- biome_area %>%
  left_join(biome_pa_area, by = "BIOME_NAME") %>%
  mutate(
    protected_area = drop_units(protected_area),
    protected_area = coalesce(protected_area, 0),
    protected_area = set_units(protected_area, "km^2"),
    percent_protected = 100 * protected_area / total_area
  )

# ===============================
#       BIOME AREA BARPLOT
# ===============================
ggplot(biome_area_df) +
  geom_col(
    aes(x = BIOME_NAME, y = drop_units(total_area)),
    fill = "grey80"
  ) +
  geom_col(
    aes(x = BIOME_NAME, y = drop_units(protected_area)),
    fill = "#1b9e77"
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Area (km²)",
    title = " "
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.22, "cm"),
    axis.text = element_text(colour = "black")
  )

