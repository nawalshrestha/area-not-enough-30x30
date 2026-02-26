library(sf)
library(terra)
library(dplyr)
library(ggplot2)

grid_file <- "10km_grid.shp"
pa_file   <- "Nepal_PA_Behrmann.shp"
elev_file <- "nepal_srtm.tif"

#===========================================
# Read spatial data
#===========================================
grid_sf <- st_read(grid_file, quiet = TRUE) |> st_transform(6933)
pa_sf   <- st_read(pa_file, quiet = TRUE) |> st_transform(6933)

elev <- rast(elev_file) |> project("EPSG:6933")

#===========================================
# Assign PA status to grid
#===========================================
grid_pa <- st_join(grid_sf, pa_sf["NAME_ENG"], left = TRUE) |>
  mutate(PA = ifelse(is.na(NAME_ENG), "Unprotected", "Protected")) |>
  select(Gridcode, PA)

#===========================================
# Extract mean elevation per grid
#===========================================
grid_vect <- vect(grid_sf)

grid_sf$elevation <- terra::extract(
  elev, grid_vect, fun = mean, na.rm = TRUE
)[,2]

#===========================================
# Dataframe analysis
#===========================================
df <- grid_sf |>
  st_drop_geometry() |>
  left_join(grid_pa, by = "Gridcode") |>
  filter(!is.na(elevation))


#===========================================
# Representation ratio by elevation band
#===========================================

elev_breaks <- seq(0, 9000, by = 1000)
elev_labels <- paste0(
  elev_breaks[-length(elev_breaks)],
  "–",
  elev_breaks[-1]
)

df <- df |>
  mutate(
    elev_band = cut(
      elevation,
      breaks = elev_breaks,
      labels = elev_labels,
      include.lowest = TRUE,
      right = FALSE
    )
  )
#===========================================
# Summarise protected vs unprotected
#===========================================

rep_df <- df |>
  count(elev_band, PA) |>
  group_by(elev_band) |>
  mutate(prop = n / sum(n)) |>
  ungroup()


#===========================================
# Stacked barplot
#===========================================
rep_df$PA <- factor(rep_df$PA, levels = c("Unprotected", "Protected"))

ggplot(rep_df,
       aes(x = elev_band, y = prop, fill = PA)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Protected"   = "#1b9e77",
      "Unprotected" = "gray80"
    ),
    name = NULL
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Elevation band (m)",
    y = "Proportion of land",
    title = "Elevational bias in Nepal’s protected area network"
  ) +
  theme_minimal(base_size = 13) +
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
