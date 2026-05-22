# stacked-bar-intra-decade-crowding-contributions.R
# Stacked bars of intra-decade crowding contributions

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "five-decade-aggregates/output/raw"
out_dir  <- "five-decade-aggregates/output"

# ----------------------------
# Read data
# ----------------------------
crowding_contrib <- read_csv(
  file.path(data_dir, "intra_decade_crowding_contributions.csv"),
  show_col_types = FALSE
)

# ----------------------------
# Prep for plotting
# ----------------------------
plot_data <- crowding_contrib |>
  filter(!is.na(decade_label)) |>
  select(
    decade_label,
    contrib_percent_hhsize,
    contrib_percent_bedroom
  ) |>
  pivot_longer(
    cols = starts_with("contrib_percent_"),
    names_to = "component",
    values_to = "contribution"
  ) |>
  mutate(
    component = recode(
      component,
      contrib_percent_bedroom = "Increased bedrooms",
      contrib_percent_hhsize  = "Decreased household size"
    ),
    contribution = contribution * 100,  # convert to percent
    decade_label = factor(decade_label, levels = unique(decade_label))
  )

# ----------------------------
# Plot
# ----------------------------
p <- ggplot(
  plot_data,
  aes(
    x = decade_label,
    y = contribution,
    fill = component
  )
) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = percent_format(scale = 1)
  ) +
  scale_fill_manual(
    values = c(
      "Increased bedrooms"        = "grey70",
      "Decreased household size"  = "white"
    )
  ) +
  labs(
    x = NULL,
    y = "Percent of crowding reduction",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(size = 10)
  )

print(p)

# ----------------------------
# Save
# ----------------------------
ggsave(
  file.path(out_dir, "stacked-bar-intra-decade-crowding-contributions.png"),
  plot = p,
  width = 6,
  height = 6,
  dpi = 500
)

# ----------------------------
# Presentation version
# ----------------------------
p_presentation <- p +
  labs(y = "Percent of\ncrowding\nreduction") +
  scale_x_discrete(
    labels = c(
      "1970-1980" = "1970 - 1980",
      "1980-1990" = "1980 - 1990",
      "1990-2000" = "1990 - 2000",
      "2000-2012" = "2000 - 2010",
      "2012-2022" = "2010 - 2020"
    )
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 1,
      margin = margin(r = 10),
      size = 18
    ),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(
  file.path(out_dir, "stacked-bar-intra-decade-crowding-contributions-presentation.png"),
  plot = p_presentation,
  width = 13,
  height = 6,
  dpi = 300
)