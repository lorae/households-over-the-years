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
  "five-decade-aggregates/output/raw/hhsize-buckets-race-decade.csv"
)

# -----------------------------
# Step 2: Filter to 1970 & 2020
# -----------------------------

fig_data <- hhsize_race_year |>
  filter(YEAR %in% c(1970, 2020)) |>
  arrange(YEAR, race_eth, NUMPREC)

# -----------------------------
# Step 4: Plot Function
# -----------------------------

plot_double_hist <- function(data, 
                             title = NULL,
                             ymax = 60, 
                             show_x = FALSE,
                             add_legend = FALSE) {
  
  d1 <- data |> 
    filter(YEAR == 1970) |>
    mutate(YEAR_label = "1970")
  
  d2 <- data |> 
    filter(YEAR == 2020) |>
    mutate(YEAR_label = "2020")
  
  ggplot(mapping = aes(x = factor(NUMPREC), y = percent)) +
    geom_bar(
      data = d1,
      aes(fill = YEAR_label, color = YEAR_label),
      stat = "identity",
      alpha = 0.4,
      width = 0.9,
      size = 0.3
    ) +
    geom_bar(
      data = d2,
      aes(fill = YEAR_label, color = YEAR_label),
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
    scale_y_continuous(
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = title,
      x = if (show_x) "Number of Persons in Household" else NULL,
      y = NULL,
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

ymax <- 35

white <- plot_double_hist(
  fig_data |> filter(race_eth == "White"),
  title = "White",
  ymax = ymax,
  show_x = FALSE
)

hispanic <- plot_double_hist(
  fig_data |> filter(race_eth == "Hispanic"),
  title = "Hispanic",
  ymax = ymax,
  show_x = FALSE
)

black <- plot_double_hist(
  fig_data |> filter(race_eth == "Black"),
  title = "Black",
  ymax = ymax,
  show_x = TRUE,
  add_legend = TRUE
)

fig <- white / hispanic / black



fig

ggsave(
  "five-decade-aggregates/output/stacked-hist-hhsize-bucket-race-1970-2020.png",
  plot = fig,
  width = 4,
  height = 6,
  units = "in",
  dpi = 300
)
