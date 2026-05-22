# regression-1970.R
#
# Fit the 1970 household-level OLS regression for the KOB decomposition.
# Uses survey::svydesign + svyglm with CLUSTER/STRATA for Taylor-series
# SEs (same approach as the published paper's 2000 regression).
#
# Inputs:
# - bedroom-allocation/throughput/kob-households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/throughput/regressions/1970.rds

library(dplyr)
library(duckdb)
library(DBI)
library(survey)
library(broom)

# ----- Step 1: Load 1970 households -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

hhs_1970 <- tbl(con, "households") |>
  filter(decade == 1970L, !is.na(bedrooms_recode)) |>
  collect()

dbDisconnect(con)

# ----- Step 2: Build survey design (Taylor linearization) -----
design_1970 <- svydesign(
  ids     = ~CLUSTER,
  strata  = ~STRATA,
  weights = ~HHWT,
  data    = hhs_1970,
  nest    = TRUE
)

# ----- Step 3: Fit regression -----
model_1970 <- svyglm(
  bedrooms_recode ~
    hoh_age + hoh_race_eth + hoh_educ_bucket + hoh_us_born +
    hoh_sex + tenure + region4 +
    n_children_under_18 + n_adults + hhincome_2020_harmonized,
  design = design_1970
)

# ----- Step 4: Save coefs + vcov -----
output_dir <- "bedroom-allocation/throughput/regressions"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(
  list(
    coefs = broom::tidy(model_1970),
    vcov  = vcov(model_1970),
    n     = nobs(model_1970)
  ),
  file = file.path(output_dir, "1970.rds")
)
