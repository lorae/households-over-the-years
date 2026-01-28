library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "output/five-decade-tables/raw"

# ----------------------------
# Read crowding change data
# ----------------------------
crowding_change <- read_csv(
  file.path(data_dir, "crowded_race_age_change_1970_2020.csv"),
  show_col_types = FALSE
)

# ----------------------------
# Ordered age buckets (NEW scheme)
# ----------------------------
age_levels <- c(
  "17 or younger",
  "18-29",
  "30-49",
  "50-65",
  "65 and older"
)

# ----------------------------
# Race order (NO Multiracial)
# ----------------------------
race_order <- c(
  "All",
  "AAPI",
  "AIAN",
  "Black",
  "Hispanic",
  "White"
)

plot_data <- crowding_change |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    race_eth   = factor(race_eth, levels = race_order)
  ) |>
  filter(!is.na(race_eth))

# ----------------------------
# Split into left / right panels
# ----------------------------
left_races  <- race_order[1:3]
right_races <- race_order[4:6]

left_data  <- plot_data |> filter(race_eth %in% left_races)
right_data <- plot_data |> filter(race_eth %in% right_races)

# ----------------------------
# Left plot (facet labels on LEFT)
# ----------------------------
left_plot <- ggplot(
  left_data,
  aes(x = age_bucket,
      y = change_pct_crowded,
      fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(
    rows = vars(race_eth),
    switch = "y"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = seq(-50, 0, by = 10),
    limits = c(-50, 0),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(labels = function(x) paste0(x, "%"))
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      size = 10
    ),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

# ----------------------------
# Right plot (facet labels on RIGHT)
# ----------------------------
right_plot <- ggplot(
  right_data,
  aes(x = age_bucket,
      y = change_pct_crowded)
) +
  geom_col(fill = "steelblue") +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(rows = vars(race_eth)) +
  scale_y_continuous(
    breaks = seq(-50, 0, by = 10),
    limits = c(-50, 0)
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(
      angle = 0,
      hjust = 0,
      size = 10
    ),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  labs(x = NULL, y = NULL)

# ----------------------------
# Combine + save
# ----------------------------
final_plot <- left_plot + right_plot +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Change in Crowding by Age Group, 1970–2020",
    subtitle = "Change in share of population living in crowded households"
  )

ggsave(
  "output/five-decade-tables/crowding_changes_race_age_1970_2020.png",
  plot = final_plot,
  width = 6.5,
  height = 6.5,
  dpi = 500
)

print(final_plot)