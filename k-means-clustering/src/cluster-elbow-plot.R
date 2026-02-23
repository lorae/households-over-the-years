# cluster-elbow-plot.R
# 
# Goal: Determine optimal k for downstream analysis
# Saves elbow plot as fig05 (in output/) as well as csv of data producing
# plot and the raw results from k = 1 through k = 15
#

# ----- Step 0: Configuration ----- #
library(dplyr)
library(duckdb)
library(dbplyr)
library(ggplot2)
library(readr)
library(purrr)
library(tidyr)
library(forcats)

devtools::load_all("../demographr")

# Create output folder if missing
out_dir <- "k-means-clustering/output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Connect to database
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

set.seed(123)

# ----- Step 1: Prepare data ----- #
vars <- c(
  "n_spouse",
  "n_child",
  "n_parent",
  "n_grandchild",
  "n_other_rel",
  "n_non_rel"
)

ipums_household_tb <- ipums_household |>
  filter(GQ %in% c(0, 1, 2)) |>
  select(YEAR, HHWT, all_of(vars)) |>
  collect()

ipums_household_scaled <- scale(ipums_household_tb |> select(-YEAR, -HHWT))

# ----- Step 2: Run all models for k = 1 to k = 15 ----- #
k_seq <- 1:15
kmeans_results <- vector("list", length(k_seq))

# TODO: refactor this so the kmeans_results_clean is not needed
for (k in k_seq) {
  message("Running k-means for k = ", k, " ...")
  km <- kmeans(ipums_household_scaled, centers = k, nstart = 10)
  kmeans_results[[as.character(k)]] <- km # <- TO FIX, likely `kmeans_results[[k]] <- km  
  message("Completed k = ", k)
}

# Keep only the non-null named elements
# TODO: fix marked line above in loop to make this unnecessary
kmeans_results_clean <- kmeans_results[!sapply(kmeans_results, is.null)]


# ----- Step 3: Compute within-cluster SS (for elbow plot) ----- #
wss <- sapply(kmeans_results_clean, function(x) x$tot.withinss)
elbow_df <- data.frame(k = k_seq, wss = wss)

# ----- Step 4: Save elbow plot and results ----- #
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Elbow plot
elbow_path <- file.path(out_dir, paste0("figures/fig05_elbow_plot_", timestamp, ".png"))
ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  labs(
    title = "Elbow Method for K-Means",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal(base_size = 14)
ggsave(elbow_path, width = 7, height = 5, dpi = 300)
message("✅ Elbow plot saved to: ", elbow_path)

# Elbow table 
csv_path <- file.path(out_dir, paste0("figure-data/fig05_elbow_plot_", timestamp, ".csv"))
write_csv(elbow_df, csv_path)
message("✅ Elbow data saved to: ", csv_path)

# All k-means results
rds_path <- file.path(paste0("k-means-clustering/throughput/k1_k15_results", timestamp, ".rds"))
saveRDS(kmeans_results, rds_path)
message("✅ K-means models saved to: ", rds_path)

# ----- Step 5: Cleanup ----- #
dbDisconnect(con)
message("Done! All results saved in ", out_dir)
