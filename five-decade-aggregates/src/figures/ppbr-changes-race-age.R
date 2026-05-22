library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "five-decade-aggregates/output/raw"
out_dir  <- "five-decade-aggregates/output"

# ----------------------------
# Read data
# ----------------------------
ppbr_change <- read_csv(
  file.path(data_dir, "ppbr_race_age_change_1970_2020.csv"),
  show_col_types = FALSE
)

hhsize_change <- read_csv(
  file.path(data_dir, "hhsize_race_age_change_1970_2020.csv"),
  show_col_types = FALSE
)

# ----------------------------
# Ordered factors
# ----------------------------
age_levels <- c(
  "17 or younger",
  "18-29",
  "30-49",
  "50-65",
  "65 and older"
)

race_order <- c(
  "All",
  "AAPI",
  "AIAN",
  "Black",
  "Hispanic",
  "White"
)

# ----------------------------
# Prep data
# ----------------------------
ppbr_change <- ppbr_change |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    race_eth   = factor(race_eth, levels = race_order)
  ) |>
  filter(!is.na(race_eth))

hhsize_change <- hhsize_change |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    race_eth   = factor(race_eth, levels = race_order)
  ) |>
  filter(!is.na(race_eth))

# ----------------------------
# Split into left/right panels
# ----------------------------
left_races  <- c("All", "AIAN", "Hispanic")
right_races <- c("AAPI", "Black", "White")

ppbr_left <- ppbr_change |>
  filter(race_eth %in% left_races) |>
  mutate(race_eth = factor(race_eth, levels = left_races))

ppbr_right <- ppbr_change |>
  filter(race_eth %in% right_races) |>
  mutate(race_eth = factor(race_eth, levels = right_races))

hhsize_left <- hhsize_change |>
  filter(race_eth %in% left_races) |>
  mutate(race_eth = factor(race_eth, levels = left_races))

hhsize_right <- hhsize_change |>
  filter(race_eth %in% right_races) |>
  mutate(race_eth = factor(race_eth, levels = right_races))

# ----------------------------
# PPBR Percent Change Plot
# ----------------------------
ppbr_pct_left <- ggplot(
  ppbr_left,
  aes(x = age_bucket, y = pct_change_ppbr, fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(rows = vars(race_eth), switch = "y") +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = seq(0, -50, by = -10),
    limits = c(-50, 0),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(labels = function(x) paste0(x, "%"))
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

ppbr_pct_right <- ggplot(
  ppbr_right,
  aes(x = age_bucket, y = pct_change_ppbr)
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
    breaks = seq(0, -50, by = -10),
    limits = c(-50, 0),
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  labs(x = NULL, y = NULL)

ppbr_pct_plot <- ppbr_pct_left + ppbr_pct_right + plot_layout(widths = c(1, 1))

ggsave(
  file.path(out_dir, "ppbr_changes_race_age_1970_2020_pct.png"),
  plot = ppbr_pct_plot,
  width = 6.5,
  height = 8,
  dpi = 500
)

# ----------------------------
# PPBR Absolute Change Plot
# ----------------------------
ppbr_abs_left <- ggplot(
  ppbr_left,
  aes(x = age_bucket, y = change_ppbr, fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(rows = vars(race_eth), switch = "y") +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = seq(0, -1.6, by = -0.5),
    limits = c(-1.6, 0),
    sec.axis = dup_axis()
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

ppbr_abs_right <- ggplot(
  ppbr_right,
  aes(x = age_bucket, y = change_ppbr)
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
    breaks = seq(0, -1.6, by = -0.5),
    limits = c(-1.6, 0)
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  labs(x = NULL, y = NULL)

ppbr_abs_plot <- ppbr_abs_left + ppbr_abs_right + plot_layout(widths = c(1, 1))

ggsave(
  file.path(out_dir, "ppbr_changes_race_age_1970_2020_abs.png"),
  plot = ppbr_abs_plot,
  width = 6.5,
  height = 8,
  dpi = 500
)

# ----------------------------
# Household Size Percent Change Plot
# ----------------------------
hhsize_pct_left <- ggplot(
  hhsize_left,
  aes(x = age_bucket, y = pct_change_hhsize, fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(rows = vars(race_eth), switch = "y") +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = seq(-40, 10, by = 10),
    limits = c(-40, 10),
    labels = function(x) paste0(x, "%"),
    sec.axis = dup_axis(labels = function(x) paste0(x, "%"))
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

hhsize_pct_right <- ggplot(
  hhsize_right,
  aes(x = age_bucket, y = pct_change_hhsize)
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
    breaks = seq(-40, 10, by = 10),
    limits = c(-40, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  labs(x = NULL, y = NULL)

hhsize_pct_plot <- hhsize_pct_left + hhsize_pct_right + plot_layout(widths = c(1, 1))

ggsave(
  file.path(out_dir, "hhsize_changes_race_age_1970_2020_pct.png"),
  plot = hhsize_pct_plot,
  width = 6.5,
  height = 8,
  dpi = 500
)

# ----------------------------
# Household Size Percent Change — Presentation version (3 cols x 2 rows)
# Horizontal gridlines flow continuously across the row (panel.spacing.x = 0).
# Bars are separated visually via extra x-axis padding inside each panel.
# Facet titles are drawn as in-panel text (overlapping gridlines).
# ----------------------------
race_label_data <- data.frame(
  race_eth = factor(
    c("All", "AAPI", "AIAN", "Black", "Hispanic", "White"),
    levels = race_order
  )
)

hhsize_pct_presentation <- ggplot(
  hhsize_change,
  aes(x = age_bucket, y = pct_change_hhsize, fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_text(
    data = race_label_data,
    aes(label = race_eth),
    x = 3, y = 6,
    inherit.aes = FALSE,
    hjust = 0.5, vjust = 0.5,
    size = 8, fontface = "bold",
    color = "grey20"
  ) +
  facet_wrap(~ race_eth, nrow = 2, ncol = 3) +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = c(
      "17 or younger" = "< 17",
      "18-29" = "18 - 29",
      "30-49" = "30 - 49",
      "50-65" = "50 - 64",
      "65 and older" = "65+"
    )
  ) +
  scale_y_continuous(
    breaks = seq(-40, 10, by = 10),
    limits = c(-40, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(add = c(4, 4))
  ) +
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    panel.spacing.x = unit(-2, "pt"),
    panel.spacing.y = unit(1.5, "lines"),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.4,
      lineend = "square"
    ),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  labs(x = NULL, y = NULL)

ggsave(
  file.path(out_dir, "hhsize_changes_race_age_1970_2020_pct-presentation.png"),
  plot = hhsize_pct_presentation,
  width = 13,
  height = 8,
  dpi = 300
)

# ----------------------------
# Household Size Absolute Change Plot
# ----------------------------
hhsize_abs_left <- ggplot(
  hhsize_left,
  aes(x = age_bucket, y = change_hhsize, fill = race_eth == "All")
) +
  geom_col() +
  geom_vline(
    aes(xintercept = as.numeric(age_bucket)),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  facet_grid(rows = vars(race_eth), switch = "y") +
  scale_fill_manual(
    values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = seq(-2, 0.5, by = 0.5),
    limits = c(-2, 0.5),
    sec.axis = dup_axis()
  ) +
  theme_minimal() +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(x = NULL, y = NULL)

hhsize_abs_right <- ggplot(
  hhsize_right,
  aes(x = age_bucket, y = change_hhsize)
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
    breaks = seq(-2, 0.5, by = 0.5),
    limits = c(-2, 0.5)
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0, size = 10),
    strip.placement = "outside",
    panel.spacing.y = unit(0.25, "lines"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(5, 5, 5, -10)
  ) +
  labs(x = NULL, y = NULL)

hhsize_abs_plot <- hhsize_abs_left + hhsize_abs_right + plot_layout(widths = c(1, 1))

ggsave(
  file.path(out_dir, "hhsize_changes_race_age_1970_2020_abs.png"),
  plot = hhsize_abs_plot,
  width = 6.5,
  height = 8,
  dpi = 500
)

print("All 4 plots saved successfully!")