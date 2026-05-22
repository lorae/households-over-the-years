# regression-2020.R
#
# Fit the 2020 household-level OLS regression for the KOB decomposition.
# Uses the matrix-algebra regression backend + parallel bootstrap over REPWT
# replicate weights for SDR variance estimation. Much faster than sequential
# svyglm over svrepdesign (runtime ~3-5 min on 10 workers vs 20-40 min).
#
# Inputs:
# - bedroom-allocation/throughput/kob-households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/throughput/regressions/2020.rds

library(dplyr)
library(duckdb)
library(DBI)
library(Matrix)
library(tibble)
library(purrr)
library(furrr)
library(future)
library(glue)
library(devtools)

load_all("../demographr")  # bootstrap_replicates_parallel, se_from_bootstrap

# Inline fast regression backend (ported from
# household-size-demographics/src/utils/regression-backends.R)
dataduck_reg_matrix_2 <- function(data, wt_col, formula) {
  mf <- model.frame(formula, data, na.action = na.omit)
  X  <- model.matrix(formula, mf) |> as("dgCMatrix")
  y  <- model.response(mf)
  wts_full <- data[[wt_col]]
  wts <- wts_full[as.integer(rownames(mf))]
  if (any(is.na(wts))) stop("Weights contain NA.")
  if (any(wts < 0)) {
    warning(glue("Negative weights in '{wt_col}'; replacing with 0."))
    wts <- pmax(wts, 0)
  }
  W  <- Diagonal(x = sqrt(wts))
  Xw <- W %*% X
  yw <- W %*% y
  coef_vec <- solve(crossprod(Xw), crossprod(Xw, yw))
  tibble(term = rownames(coef_vec), estimate = as.numeric(coef_vec)) |>
    arrange(term)
}

# ----- Step 1: Load 2020 households -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

hhs_2020 <- tbl(con, "households") |>
  filter(decade == 2020L, !is.na(bedrooms_recode)) |>
  collect()

dbDisconnect(con)

# ----- Step 2: Find REPWT columns -----
repwt_cols <- grep("^REPWT[0-9]+$", names(hhs_2020), value = TRUE)
stopifnot(length(repwt_cols) == 80)

# ----- Step 3: Parallel plan (multisession works on Windows) -----
options(future.globals.maxSize = 20 * 1024^3)   # 20 GiB per worker
plan(multisession, workers = 2)                  # bump if RAM allows

# ----- Step 4: Run bootstrap replicates -----
formula <- bedrooms_recode ~
  hoh_age + hoh_race_eth + hoh_educ_bucket + hoh_us_born +
  hoh_sex + tenure + region4 +
  n_children_under_18 + n_adults + hhincome_2020_harmonized

bootstrap_results <- bootstrap_replicates_parallel(
  data       = hhs_2020,
  f          = dataduck_reg_matrix_2,
  wt_col     = "HHWT",
  repwt_cols = repwt_cols,
  id_cols    = "term",
  formula    = formula,
  verbose    = TRUE
)

# ----- Step 5: Compute SEs via SDR formula -----
model_output <- se_from_bootstrap(
  bootstrap = bootstrap_results,
  constant  = 4 / 80,
  se_cols   = c("estimate")
)

# ----- Step 6: Save -----
output_dir <- "bedroom-allocation/throughput/regressions"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(
  list(
    coefs = model_output,   # tibble: term, estimate, se_estimate
    n     = nrow(hhs_2020)
  ),
  file = file.path(output_dir, "2020.rds")
)
