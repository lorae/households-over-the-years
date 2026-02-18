# src/figures/fig-hhsize-race-1970-2020.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(readr)
library(scales)

# -----------------------------
# Step 1: Import Data
# -----------------------------

hhsize_race_year <- read_csv(
  "output/five-decade-tables/raw/hhsize-buckets-race-decade.csv"
)

# -----------------------------
# Step 2: Filter to 1970 & 2020
# -----------------------------

hhsize_race_year <- hhsize_race_year |>
  filter(year %in% c(1970, 2020))

# -----------------------------
# Step 3: Topcode household size
# -----------------------------

topcode_hhsize <- 8

fig_data <- hhsize_race_year |>
  mutate(NUMPREC = if_else(NUMPREC >= topcode_hhsize, 
                           topcode_hhsize, 
                           NUMPREC)) |>
  group_by(RACE_ETH_bucket, NUMPREC, year) |>
  summarise(
    weighted_count = sum(weighted_count),
    count = sum(count),
    .groups = "drop"
  ) |>
  group_by(RACE_ETH_bucket, year) |>
  mutate(freq = weighted_count / sum(weighted_count)) |>
  ungroup() |>
  arrange(year, RACE_ETH_bucket, NUMPREC)

# -----------------------------
# Step 4: Plot Function
# -----------------------------

plot_double_hist <- function(data, title = NULL, ymax = 0.35, add_legend = FALSE) {
  
  d1 <- data |> 
    filter(year == 1970) |>
    mutate(year_label = "1970")
  
  d2 <- data |> 
    filter(year == 2020) |>
    mutate(year_label = "2020")
  
  ggplot(mapping = aes(x = factor(NUMPREC), y = freq)) +
    geom_bar(
      data = d1,
      aes(fill = year_label, color = year_label),
      stat = "identity",
      alpha = 0.4,
      width = 0.9,
      size = 0.3
    ) +
    geom_bar(
      data = d2,
      aes(fill = year_label, color = year_label),
      stat = "identity",
      alpha = 0.7,
      width = 0.6,
      size = 0.3
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_fill_manual(
      values = c("1970" = "skyblue",
                 "2020" = "forestgreen"),
      guide = if (add_legend) "legend" else "none"
    ) +
    scale_color_manual(
      values = c("1970" = "skyblue",
                 "2020" = "forestgreen"),
      guide = if (add_legend) "legend" else "none"
    ) +
    labs(
      title = title,
      x = "Number of Persons in Household",
      y = "Frequency",
      fill = NULL,
      color = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = if (add_legend) "bottom" else "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
}

# -----------------------------
# Step 5: Create Plots
# -----------------------------

ymax <- 0.35

white <- plot_double_hist(
  fig_data |> filter(RACE_ETH_bucket == "White"),
  title = "White"
)

hispanic <- plot_double_hist(
  fig_data |> filter(RACE_ETH_bucket == "Hispanic"),
  title = "Hisp
