# cluster-k9-selection.R
# 
# Goal: Run model using k=9 multiple times using different seeds and choose result 
# with the lowest sum of squares
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
library("glue")

devtools::load_all("../demographr")

# Create output folder if missing
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Connect to database
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

# Initialize parameters
k <- 9
n_iter <- 10
fignum <- 11

# Initialize output paths
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
lowest_ss_data_path <- glue("output/figure-data/fig{fignum}-k{k}-model-ss-{timestamp}.csv")
lowest_ss_fig_path <- glue("output/figures/fig{fignum}-k{k}-model-ss-{timestamp}.png")
best_model_path <- glue("throughput/k{k}-best-model.rds")
labelled_cluster_path <- glue("throughput/k{k}-best-model-cluster-labels.rds")
all_models_path <- glue("throughput/k{k}-all-models-{timestamp}.rds")


# ----- Step 1: Prepare data ----- #
# TODO: potentially move step 0 and 1 to accessory script since they are identical
# between this and cluster-elbow-plot
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

# ----- Step 2: Iterate from seed = 1 to 10 for k = 6 ----- #

message("Running k-means for k = ", k, " across ", n_iter, " seeds...")

fit_results <- map_df(1:n_iter, function(seed) {
  message("Running seed ", seed, " of ", n_iter, " ...")
  set.seed(seed)
  km <- kmeans(ipums_household_scaled, centers = k, nstart = 10)
  message("  ✅ Completed seed ", seed)
  
  tibble(
    seed = seed,
    tot_withinss = km$tot.withinss,
    betweenss = km$betweenss,
    totss = km$totss,
    ratio = km$betweenss / km$totss,
    model = list(km)
  )
})

# ----- Step 3: Select best model (lowest within-cluster SS) ----- #
best_fit <- fit_results |> slice_min(tot_withinss, n = 1)
best_model <- best_fit$model[[1]]
best_seed <- best_fit$seed
message("✅ Best seed: ", best_seed, " with total within-SS = ", round(best_fit$tot_withinss, 2))

# ----- Step 4: Save outputs ----- #
# Summary table of all runs
fit_results |>
  select(-model) |>
  write_csv(lowest_ss_data_path)
message("✅ Summary results saved to: ", lowest_ss_data_path)

# Summary graph of all runs
lowest_ss <- ggplot(fit_results, aes(x = seed, y = tot_withinss)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#0072B2", size = 2) +
  geom_point(
    data = best_fit,
    aes(x = seed, y = tot_withinss),
    color = "red", size = 3
  ) +
  labs(
    title = "Total Within-Cluster SS by Seed (k = 9)",
    x = "Random Seed",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal(base_size = 14)
lowest_ss
ggsave(lowest_ss_fig_path, width = 6, height = 4, dpi = 300)
message("Diagnostic plot saved to: ", lowest_ss_fig_path)
        
# Best-fit model object
saveRDS(best_model, best_model_path)
message("Best-fit model saved to: ", best_model_path)

# Raw data with cluster labels
labelled_clusters <- ipums_household_tb |> mutate(cluster = k_fit$cluster)
saveRDS(labelled_clusters, labelled_cluster_path)
message("Labelled clusters saved to: ", labelled_cluster_path)

# All models
saveRDS(fit_results, all_models_path)
message("All model runs saved to: ", all_models_path)


# ----- Step 5: Cleanup ----- #
dbDisconnect(con)
message("Done!")
