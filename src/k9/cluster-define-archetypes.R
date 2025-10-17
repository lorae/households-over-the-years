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
k_fit <- readRDS("throughput/k9-best-model.rds")
k_data <- readRDS("throughput/k9-best-model-cluster-labels.rds")

# Initialize output paths
cluster_centers_path <- "output/tables/tab04-k9-cluster-zscores.csv"
cluster_means_path <- "output/tables/tab05-k9-cluster-means.csv"
cluster_90pct_intervals_path <- "output/tables/tab06-k9-cluster-90pct-intervals.csv"

# ----- Step 1: get z-score cluster centers ----- #
# Cluster centers (z scores)
cluster_centers <- round(k_fit$centers, 2)

# ----- Step 2: Get mean cluster centers ----- #
# in units of persons

vars <- c("n_spouse", "n_child", "n_parent", "n_grandchild", "n_other_rel", "n_non_rel")

get_cluster_mean <- function(data, var_name) {
  crosstab_mean(
    data,
    value = var_name,
    wt_col = "HHWT",
    group_by = "cluster"
  ) |>
    select(cluster, weighted_mean) |>
    rename(!!var_name := weighted_mean)
}

cluster_means <- vars |>
  map(~ get_cluster_mean(k_data, .x)) |>
  reduce(left_join, by = "cluster")
  
# ----- Step 3: Get 5th percentile and 95th percentile within each cluster ----- #
# Weighted quantile helper (internal)
weighted_quantile <- function(x, w, probs = c(0.05, 0.95)) {
  df <- tibble(x = x, w = w) |> filter(!is.na(x) & !is.na(w))
  df <- df |> arrange(x) |> mutate(w_cum = cumsum(w) / sum(w))
  sapply(probs, function(p) {
    idx <- which.min(abs(df$w_cum - p))
    df$x[idx]
  })
}

# Function to get weighted percentiles for a single variable
get_cluster_percentile <- function(data, var_name) {
  data |>
    group_by(cluster) |>
    summarise(
      p5  = weighted_quantile(.data[[var_name]], HHWT, probs = 0.05)[1],
      p95 = weighted_quantile(.data[[var_name]], HHWT, probs = 0.95)[1],
      .groups = "drop"
    ) |>
    rename(
      !!paste0(var_name, "_p5")  := p5,
      !!paste0(var_name, "_p95") := p95
    )
}

# Apply to all vars and merge into one wide table
cluster_percentile_list <- vars |>
  map(~ get_cluster_percentile(k_data, .x))


# ----- Step 4: Export these tables ----- #

# Export z-scores (cluster centers)
cluster_centers_df <- as.data.frame(cluster_centers)
cluster_centers_df <- cluster_centers_df |>
  mutate(cluster = row_number(), .before = 1)

write_csv(cluster_centers_df, cluster_centers_path)

# Export means
write_csv(cluster_means, cluster_means_path)

# Export 90% intervals (5th to 95th percentile)
# Merge all percentile tables
cluster_percentiles <- cluster_percentile_list |>
  reduce(left_join, by = "cluster")

# Reshape to show (low, high) format for each variable
cluster_intervals <- cluster_percentiles |>
  mutate(cluster = cluster, .before = 1)

# Create a version with formatted intervals
cluster_intervals_formatted <- tibble(cluster = cluster_percentiles$cluster)

for (var in vars) {
  p5_col <- paste0(var, "_p5")
  p95_col <- paste0(var, "_p95")
  
  cluster_intervals_formatted <- cluster_intervals_formatted |>
    mutate(
      !!var := paste0("(", cluster_percentiles[[p5_col]], ", ", 
                      cluster_percentiles[[p95_col]], ")")
    )
}

write_csv(cluster_intervals_formatted, cluster_90pct_intervals_path)

# Cluster sizes
cluster_sizes <- k_fit$size

