# fig01-bedroom-dist-by-tenure-1970-2020.R
#
# Coauthor request: bedroom-distribution version of the race-faceted hhsize
# histogram in five-decade-aggregates. Full-population household-level,
# faceted by tenure (renter vs owner), overlaying 1970 and 2020.
#
# Inputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/output/figures/fig01-bedroom-dist-by-tenure-1970-2020.png
# - bedroom-allocation/output/figure-data/fig01-bedroom-dist-by-tenure-1970-2020.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
library(patchwork)
devtools::load_all("../demographr")

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb",
  read_only = TRUE
)
households <- tbl(con, "households")

# --- 1970 & 2020 only ---

fig_data <- households |>
  filter(decade %in% c(1970L, 2020L)) |>
  crosstab_percent(
    wt_col = "HHWT",
    group_by = c("decade", "tenure", "bedrooms_recode"),
    percent_group_by = c("decade", "tenure")
  )

dbDisconnect(con)

# --- overlapping histogram plot, one panel per tenure ---

plot_double_hist <- function(data,
                             title = NULL,
                             ymax = 60,
                             show_x = FALSE,
                             add_legend = FALSE) {

  d1 <- data |> filter(decade == 1970L) |> mutate(decade_label = "1970")
  d2 <- data |> filter(decade == 2020L) |> mutate(decade_label = "2020")

  ggplot(mapping = aes(x = factor(bedrooms_recode), y = percent)) +
    geom_bar(
      data = d1,
      aes(fill = decade_label, color = decade_label),
      stat = "identity",
      alpha = 0.4,
      width = 0.9,
      size = 0.3
    ) +
    geom_bar(
      data = d2,
      aes(fill = decade_label, color = decade_label),
      stat = "identity",
      alpha = 0.7,
      width = 0.6,
      size = 0.3
    ) +
    coord_cartesian(ylim = c(0, ymax)) +
    scale_fill_manual(
      values = c("1970" = "skyblue", "2020" = "forestgreen"),
      guide = if (add_legend) "legend" else "none"
    ) +
    scale_color_manual(
      values = c("1970" = "skyblue", "2020" = "forestgreen"),
      guide = if (add_legend) "legend" else "none"
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = title,
      x = if (show_x) "Number of Bedrooms (0 = studio, 5 = 5+)" else NULL,
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

ymax <- 45

renter_plot <- plot_double_hist(
  fig_data |> filter(tenure == "renter"),
  title = "Renter",
  ymax = ymax,
  show_x = FALSE
)

owner_plot <- plot_double_hist(
  fig_data |> filter(tenure == "owner"),
  title = "Owner",
  ymax = ymax,
  show_x = TRUE,
  add_legend = TRUE
)

fig <- renter_plot / owner_plot

fig

ggsave(
  "bedroom-allocation/output/figures/fig01-bedroom-dist-by-tenure-1970-2020.png",
  plot = fig,
  width = 4,
  height = 6,
  units = "in",
  dpi = 300
)

write.csv(
  fig_data,
  "bedroom-allocation/output/figure-data/fig01-bedroom-dist-by-tenure-1970-2020.csv",
  row.names = FALSE
)
