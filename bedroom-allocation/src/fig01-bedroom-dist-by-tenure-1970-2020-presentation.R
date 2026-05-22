# fig01-bedroom-dist-by-tenure-1970-2020-presentation.R
#
# Presentation versions of fig01:
#   (1) Renter | Owner side-by-side, 1970 vs 2020 overlay
#   (2) All households combined (no tenure split), 1970 vs 2020 overlay

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

fig_data <- households |>
  filter(decade %in% c(1970L, 2020L)) |>
  crosstab_percent(
    wt_col = "HHWT",
    group_by = c("decade", "tenure", "bedrooms_recode"),
    percent_group_by = c("decade", "tenure")
  )

fig_data_all <- households |>
  filter(decade %in% c(1970L, 2020L)) |>
  crosstab_percent(
    wt_col = "HHWT",
    group_by = c("decade", "bedrooms_recode"),
    percent_group_by = c("decade")
  )

dbDisconnect(con)

plot_double_hist <- function(data,
                             title = NULL,
                             ymax = 60,
                             show_x = TRUE,
                             show_y = TRUE,
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
      linewidth = 0.3
    ) +
    geom_bar(
      data = d2,
      aes(fill = decade_label, color = decade_label),
      stat = "identity",
      alpha = 0.7,
      width = 0.6,
      linewidth = 0.3
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
    scale_x_discrete(
      labels = c("0" = "Studio", "5" = "5+")
    ) +
    labs(
      title = title,
      x = if (show_x) "Number of bedrooms" else NULL,
      y = NULL,
      fill = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
      legend.position = if (add_legend) "right" else "none",
      legend.text = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = if (show_y) element_text(size = 16) else element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(10, 15, 10, 10)
    )
}

ymax <- 45

# --- Plot 1: Renter | Owner side-by-side ---
renter_plot <- plot_double_hist(
  fig_data |> filter(tenure == "renter"),
  title = "Renter",
  ymax = ymax,
  show_x = TRUE,
  show_y = TRUE,
  add_legend = FALSE
)

owner_plot <- plot_double_hist(
  fig_data |> filter(tenure == "owner"),
  title = "Owner",
  ymax = ymax,
  show_x = TRUE,
  show_y = FALSE,
  add_legend = TRUE
)

fig_tenure <- renter_plot + owner_plot + plot_layout(widths = c(1, 1))

ggsave(
  "five-decade-aggregates/output/fig01-bedroom-dist-by-tenure-1970-2020-presentation.png",
  plot = fig_tenure,
  width = 14,
  height = 6,
  units = "in",
  dpi = 300
)

# --- Plot 2: All households combined ---
all_plot <- plot_double_hist(
  fig_data_all,
  title = NULL,
  ymax = ymax,
  show_x = TRUE,
  show_y = TRUE,
  add_legend = TRUE
) +
  theme(
    legend.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20)
  )

ggsave(
  "five-decade-aggregates/output/fig01-bedroom-dist-all-1970-2020-presentation.png",
  plot = all_plot,
  width = 11,
  height = 6,
  units = "in",
  dpi = 300
)
