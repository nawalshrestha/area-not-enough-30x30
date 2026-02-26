library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# ===============================
# Read species-level coverage
# ===============================
data_dir <- "data"

coverage_df <- read_csv(
  file.path(data_dir, "species_partial_coverage.csv")
)

# ===============================
# Merge plant taxa
# ===============================
coverage_df <- coverage_df %>%
  mutate(
    taxon = case_when(
      taxon %in% c("liliopsida", "magnoliopsida") ~ "Plants",
      TRUE ~ taxon
    )
  )

# ====================================
# Generate ECDF-style threshold data
# ===================================
threshold_df <- coverage_df %>%
  group_by(taxon) %>%
  summarise(
    coverage_values = list(coverage_percent),
    .groups = "drop"
  ) %>%
  tidyr::expand_grid(threshold = seq(0, 100, by = 1)) %>%
  rowwise() %>%
  mutate(
    percent_species_below = mean(unlist(coverage_values) < threshold) * 100
  ) %>%
  ungroup()

# ===================================
# Factor order (clean & consistent)
# ===================================
threshold_df$taxon <- factor(
  threshold_df$taxon,
  levels = c("amphibian", "reptile", "aves", "mammal", "Plants")
)

# ===============================
#         ECDF plot
# ===============================
ggplot(
  threshold_df,
  aes(x = threshold, y = percent_species_below, color = taxon)
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "amphibian" = "#009E73",   # bluish green
      "reptile"   = "#D55E00",   # vermillion
      "mammal"    = "#0072B2",   # blue
      "aves"      = "#E69F00",   # orange
      "Plants"    = "#CC79A7"    # reddish purple
    )
  ) +
  
  # Policy thresholds
  geom_vline(xintercept = 10, linetype = "dashed", linewidth = 0.6, color = "grey40") +
  geom_vline(xintercept = 30, linetype = "dashed", linewidth = 0.6, color = "grey40") +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  
  labs(
    x = "Species' range with protected areas (%))",
    y = "Species below range protection threshold (%)",
    color = "Taxon"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black"),  # restore tick marks
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.x = element_text(size = 13, colour = "black"),
    axis.title.y = element_text(size = 13, colour = "black"),
    axis.text.x  = element_text(size = 13, colour = "black"),
    axis.text.y  = element_text(size = 13, colour = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
