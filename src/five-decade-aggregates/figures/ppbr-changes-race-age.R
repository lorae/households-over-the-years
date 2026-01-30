library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "output/five-decade-tables/raw"
out_dir  <- "output/five-decade-tables"

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

plot_data <- ppbr_change |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    race_eth   = factor(race_eth, levels = race_order)
  ) |>
  filter(!is.na(race_eth))

# ----------------------------
# Split panels
# ----------------------------
left_races  <- race_order[c(1, 3, 5)]
right_races <- race_order[c(2, 4, 6)]

left_data <- plot_data |>
  filter(race_eth %in% left_races) |>
  mutate(race_eth = factor(as.character(race_eth), levels = left_races)) |>
  arrange(race_eth, age_bucket)

right_data <- plot_data |>
  filter(race_eth %in% right_races) |>
  mutate(race_eth = factor(as.character(race_eth), levels = right_races)) |>
  arrange(race_eth, age_bucket)

# ----------------------------
# Y-axis specifications
# ----------------------------
y_specs <- list(
  pct = list(
    y_var = "pct_change_ppbr",
    y_min = -50,
    y_max = 0,
    y_by  = -10,
    suffix = "pct"
  ),
  abs = list(
    y_var = "change_ppbr",
    y_min = -1.6,
    y_max = 0,
    y_by  = -0.5,
    suffix = "abs"
  )
)

# ----------------------------
# Plot function
# ----------------------------
make_plot <- function(y_var, y_min, y_max, y_by) {
  
  # conditional label formatter
  y_labeller <- if (y_var == "pct_change_ppbr") {
    function(x) paste0(x, "%")
  } else {
    function(x) x
  }
  
  left_plot <- ggplot(
    left_data,
    aes(x = age_bucket,
        y = .data[[y_var]],
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
    facet_grid(rows = vars(race_eth), switch = "y") +
    scale_fill_manual(
      values = c("TRUE" = "grey60", "FALSE" = "steelblue"),
      guide = "none"
    ) +
    scale_y_continuous(
      breaks = seq(from = y_max, to = y_min, by = y_by),
      limits = c(y_min, y_max),
      labels = y_labeller,
      sec.axis = dup_axis(labels = y_labeller)
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
  
  right_plot <- ggplot(
    right_data,
    aes(x = age_bucket,
        y = .data[[y_var]])
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
      breaks = seq(from = y_max, to = y_min, by = y_by),
      limits = c(y_min, y_max)
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
  
  left_plot + right_plot + plot_layout(widths = c(1, 1))
}

# ----------------------------
# Build both versions
# ----------------------------
plots <- list()

for (nm in names(y_specs)) {
  spec <- y_specs[[nm]]
  
  plots[[nm]] <- make_plot(
    y_var = spec$y_var,
    y_min = spec$y_min,
    y_max = spec$y_max,
    y_by  = spec$y_by
  )
  
  # show each one for manual review
  print(plots[[nm]])
}

# ----------------------------
# Save both versions
# ----------------------------
for (nm in names(y_specs)) {
  spec <- y_specs[[nm]]

  ggsave(
    filename = file.path(
      out_dir,
      paste0(
        "ppbr_changes_race_age_1970_2020_",
        spec$suffix,
        ".png"
      )
    ),
    plot = plots[[nm]],
    width = 6.5,
    height = 8,
    dpi = 500
  )
}