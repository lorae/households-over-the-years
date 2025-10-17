# Clustering analysis
# Goal: Cluster household data to produce archetypal households
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("purrr")
library("tidyr")
library("forcats")

devtools::load_all("../demographr")

# Retrieve best fit model
k_data <- readRDS("throughput/k9-best-model-cluster-labels.rds")

# ----- Step 4: Get cluster counts by years ----- #
clusters_raw <- k_data |>
  mutate(
    cluster_name = case_when(
      cluster == 1 ~ "Small grandfamily",
      cluster == 2 ~ "Some roommates",
      cluster == 3 ~ "Nuclear family",
      cluster == 4 ~ "Single person",
      cluster == 5 ~ "Extended family",
      cluster == 6 ~ "Large grandfamily",
      cluster == 7 ~ "Adult child + parent",
      cluster == 8 ~ "Few roommates",
      cluster == 9 ~ "Many roommates"
    )
  )

cluster_counts_year <- crosstab_count(
    data = clusters_raw,
    wt_col = "HHWT",
    group_by = c("YEAR", "cluster_name")
  ) |>
  group_by(cluster_name) |> 
  tidyr::complete(
    YEAR = c(seq(1900, 2000, by = 10), 2006, 2011,2016, 2021, 2023), 
    fill = list(weighed_count = NA))
 
cluster_percents_year <- crosstab_percent(
  data = clusters_raw,
  wt_col = "HHWT",
  group_by = c("YEAR", "cluster_name"),
  percent_group_by = "YEAR"
) |>
  group_by(cluster_name) |> 
  tidyr::complete(
    YEAR = c(seq(1900, 2000, by = 10), 2006, 2011,2016, 2021, 2023), 
    fill = list(percent = NA))

cluster_percents_rel_1900 <- cluster_percents_year |>
  group_by(cluster_name) |>
  mutate(rel_percent = percent / percent[YEAR == 1900]) |>
  ungroup()

cluster_percents_rel_2023 <- cluster_percents_year |>
  group_by(cluster_name) |>
  mutate(rel_percent = percent / percent[YEAR == 2023]) |>
  ungroup()


# Reusable graphing function
plot_cluster_trends <- function(data, y_var, y_label, title, palette = "Set3") {
  ggplot(data, aes(x = YEAR, y = .data[[y_var]], color = cluster_name)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    scale_color_brewer(palette = palette, name = "Household Type") +
    scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
    labs(
      title = title,
      x = "Year",
      y = y_label
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position =)
}

# 1. Weighted counts over time
plot_counts <- plot_cluster_trends(
  data = cluster_counts_year,
  y_var = "weighted_count",
  y_label = "Weighted Household Count",
  title = "Household Archetypes Over Time"
)

# 2. Percentages over time
plot_percents <- plot_cluster_trends(
  data = cluster_percents_year,
  y_var = "percent",
  y_label = "Percent of Households",
  title = "Household Archetype Distribution Over Time"
)

# 3. Relative to 1900 baseline
plot_relative1900 <- plot_cluster_trends(
  data = cluster_percents_rel_1900,
  y_var = "rel_percent",
  y_label = "Relative to 1900 (1.0 = baseline)",
  title = "Household Archetype Trends Relative to 1900"
)

# 4. Relative to 2023 baseline
plot_relative2023 <- plot_cluster_trends(
  data = cluster_percents_rel_2023,
  y_var = "rel_percent",
  y_label = "Relative to 2023 (1.0 = baseline)",
  title = "Household Archetype Trends Relative to 2023"
)

# Display the plots
plot_counts
plot_percents
plot_relative1900
plot_relative2023

# ----- SAVE! ----- #
# Save the plots as JPEGs (6" x 6")
ggsave("output/figures/fig12-96-household-counts-time-line.jpg", 
       plot_counts, 
       width = 6, height = 6, 
       dpi = 300,
       scale = 1.25)

ggsave("output/figures/fig13-k9-household-percents-time-line.jpg", 
       plot_percents, 
       width = 6, height = 6, 
       dpi = 300,
       scale = 1.25)

ggsave("output/figures/fig14-k9-household-relative-1900-line.jpg", 
       plot_relative1900, 
       width = 6, height = 6, 
       dpi = 300,
       scale = 1.25)

ggsave("output/figures/fig15-k9-household-relative-2023-line.jpg", 
       plot_relative2023, 
       width = 6, height = 6, 
       dpi = 300,
       scale = 1.25)

# Save the CSV files in output/figure-data
write_csv(cluster_counts_year, 
          "output/figure-data/fig12-k9-household-counts-time-line.csv")

write_csv(cluster_percents_year, 
          "output/figure-data/fig13-k9-household-percents-time-line.csv")

write_csv(cluster_percents_rel_1900, 
          "output/figure-data/fig14-k9-household-relative-1900-line.csv")

write_csv(cluster_percents_rel_2023, 
          "output/figure-data/fig15-k9-household-relative-2023-line.csv")

