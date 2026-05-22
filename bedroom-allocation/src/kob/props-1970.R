# props-1970.R
#
# Compute weighted means + proportions for each KOB regressor in 1970,
# using the same Taylor-linearization survey design as regression-1970.R.
# Outputs a tidy tibble matching the regression's term naming so the two
# can be joined on `term` when building kob_input.
#
# Inputs:
# - bedroom-allocation/throughput/kob-households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/throughput/regressions/1970_props.rds

library(dplyr)
library(duckdb)
library(DBI)
library(survey)
library(purrr)
library(tibble)

# broom::tidy has no method for svystat; extract manually.
# Pattern matches household-size-demographics/src/scripts/kob-build-input.R:100.
tidy_svystat <- function(svystat_obj) {
  tibble(
    term      = names(svystat_obj),
    estimate  = as.numeric(svystat_obj),
    std.error = sqrt(diag(attr(svystat_obj, "var")))
  )
}

# ----- Step 1: Load 1970 households -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

# Match the regression's row set exactly: svyglm drops rows with NA in ANY
# regressor. Apply the same filters here so props share the same sample
# and the KOB decomposition totals balance exactly.
hhs_1970 <- tbl(con, "households") |>
  filter(
    decade == 1970L,
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

# ----- Step 2: Convert categoricals to factors BEFORE design -----
# Makes svymean term names match the regression's term names
# (e.g. "hoh_race_ethBlack" not "factor(hoh_race_eth)Black").
vars_categorical <- c("hoh_race_eth", "hoh_educ_bucket", "hoh_us_born",
                      "hoh_sex", "tenure", "region4")
vars_continuous  <- c("hoh_age", "n_children_under_18", "n_adults",
                      "hhincome_2020_harmonized")

for (v in vars_categorical) {
  hhs_1970[[v]] <- factor(hhs_1970[[v]])
}

# ----- Step 3: Build Taylor design -----
design_1970 <- svydesign(
  ids     = ~CLUSTER,
  strata  = ~STRATA,
  weights = ~HHWT,
  data    = hhs_1970,
  nest    = TRUE
)

# ----- Step 4: Compute svymean for each regressor -----
all_vars <- c(vars_continuous, vars_categorical)

props_list <- map(all_vars, function(v) {
  stat <- svymean(as.formula(paste0("~", v)), design_1970, na.rm = TRUE)
  tidy_svystat(stat)
})

props_1970 <- bind_rows(props_list)

# ----- Step 5: Save -----
output_dir <- "bedroom-allocation/throughput/regressions"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

saveRDS(props_1970, file = file.path(output_dir, "1970_props.rds"))
