# props-2020.R
#
# Compute weighted means + proportions for each KOB regressor in 2020 using
# parallel bootstrap over REPWT replicate weights (same pattern as
# regression-2020.R). Much faster than svymean over svrepdesign.
#
# Inputs:
# - bedroom-allocation/throughput/kob-households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/throughput/regressions/2020_props.rds

library(dplyr)
library(duckdb)
library(DBI)
library(tibble)
library(purrr)
library(furrr)
library(future)
library(glue)
library(devtools)

load_all("../demographr")  # bootstrap_replicates_parallel, se_from_bootstrap

# The function bootstrap_replicates_parallel will call on each weight column.
# Returns tibble with `term` and `estimate` - matching the regression
# output's convention (hoh_race_ethBlack style for categoricals).
weighted_means_and_props <- function(data, wt_col, vars_continuous, vars_categorical) {
  w <- data[[wt_col]]

  # Continuous: one weighted mean per variable
  cont_rows <- map_dfr(vars_continuous, function(v) {
    x    <- data[[v]]
    keep <- !is.na(x) & !is.na(w)
    tibble(
      term     = v,
      estimate = sum(x[keep] * w[keep]) / sum(w[keep])
    )
  })

  # Categorical: one weighted proportion per level
  cat_rows <- map_dfr(vars_categorical, function(v) {
    f      <- data[[v]]
    totals <- tapply(w, f, sum, na.rm = TRUE)
    totals <- totals[!is.na(names(totals))]
    props  <- totals / sum(totals, na.rm = TRUE)
    tibble(
      term     = paste0(v, names(props)),
      estimate = as.numeric(props)
    )
  })

  bind_rows(cont_rows, cat_rows) |> arrange(term)
}

# ----- Step 1: Load 2020 households -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

# Match the regression's row set exactly: svyglm drops rows with NA in ANY
# regressor. Apply the same filters here so props share the same sample
# and the KOB decomposition totals balance exactly.
hhs_2020 <- tbl(con, "households") |>
  filter(
    decade == 2020L,
    !is.na(bedrooms_recode),
    !is.na(hoh_age),
    !is.na(hoh_race_eth),
    !is.na(hoh_educ_bucket),
    !is.na(hoh_us_born),
    !is.na(hoh_sex),
    !is.na(tenure),
    !is.na(region4),
    !is.na(n_children_under_18),
    !is.na(n_adults),
    !is.na(hhincome_2020_harmonized)
  ) |>
  collect()

dbDisconnect(con)

vars_categorical <- c("hoh_race_eth", "hoh_educ_bucket", "hoh_us_born",
                      "hoh_sex", "tenure", "region4")
vars_continuous  <- c("hoh_age", "n_children_under_18", "n_adults",
                      "hhincome_2020_harmonized")

for (v in vars_categorical) {
  hhs_2020[[v]] <- factor(hhs_2020[[v]])
}

# ----- Step 2: Find REPWT columns -----
repwt_cols <- grep("^REPWT[0-9]+$", names(hhs_2020), value = TRUE)
stopifnot(length(repwt_cols) == 80)

# ----- Step 3: Parallel plan -----
options(future.globals.maxSize = 20 * 1024^3)
plan(multisession, workers = 4)

# ----- Step 4: Run bootstrap replicates -----
bootstrap_results <- bootstrap_replicates_parallel(
  data             = hhs_2020,
  f                = weighted_means_and_props,
  wt_col           = "HHWT",
  repwt_cols       = repwt_cols,
  id_cols          = "term",
  vars_continuous  = vars_continuous,
  vars_categorical = vars_categorical,
  verbose          = TRUE
)

# ----- Step 5: Compute SEs via SDR formula -----
props_2020 <- se_from_bootstrap(
  bootstrap = bootstrap_results,
  constant  = 4 / 80,
  se_cols   = c("estimate")
) |>
  rename(std.error = se_estimate) |>
  select(term, estimate, std.error)

# ----- Step 6: Save -----
output_dir <- "bedroom-allocation/throughput/regressions"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(props_2020, file = file.path(output_dir, "2020_props.rds"))
