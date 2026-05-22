# build-kob-input.R
#
# Combines regression coefs and population proportions from 1970 and 2020
# into a tidy kob_input tibble ready for the KOB engine.
#
# Pipeline:
#   1. Harmonize term naming + SE column names across years.
#   2. Build `adjust_by` (categorical vars + their levels) from props.
#   3. Run standardize_coefs() on each year's coefs (implicit zeros + G-U).
#   4. Join standardized coefs with props by term.
#
# Inputs:
#   bedroom-allocation/throughput/regressions/1970.rds       (coefs + vcov + n)
#   bedroom-allocation/throughput/regressions/2020.rds       (coefs + n)
#   bedroom-allocation/throughput/regressions/1970_props.rds (tidy svymean)
#   bedroom-allocation/throughput/regressions/2020_props.rds (tidy svymean)
#
# Outputs:
#   bedroom-allocation/throughput/kob_input.rds

library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(glue)

source("bedroom-allocation/src/kob/utils/regression-postprocess.R")

# ----- Step 1: Load artifacts -----
reg_1970   <- readRDS("bedroom-allocation/throughput/regressions/1970.rds")
reg_2020   <- readRDS("bedroom-allocation/throughput/regressions/2020.rds")
props_1970 <- readRDS("bedroom-allocation/throughput/regressions/1970_props.rds")
props_2020 <- readRDS("bedroom-allocation/throughput/regressions/2020_props.rds")

# ----- Step 2: Harmonize term names + SE column names -----
# 1970 regression (broom::tidy(svyglm)):       term, estimate, std.error
# 2020 regression (bootstrap SDR):             term, estimate, se_estimate
# Both wrapped STATEFIP in factor() in the formula, giving "factor(STATEFIP)6".
# Strip the factor() wrap to match props term naming ("STATEFIP6").
strip_factor <- function(x) sub("^factor\\((.*?)\\)", "\\1", x)

coefs_1970 <- reg_1970$coefs |>
  mutate(term = strip_factor(term)) |>
  select(term, estimate, std.error)

coefs_2020 <- reg_2020$coefs |>
  mutate(term = strip_factor(term)) |>
  rename(std.error = se_estimate) |>
  select(term, estimate, std.error)

# ----- Step 3: Build adjust_by from observed props (union across years) -----
categorical_vars <- c("hoh_race_eth", "hoh_educ_bucket", "hoh_us_born",
                      "hoh_sex", "tenure", "region4")

adjust_by <- bind_rows(props_1970, props_2020) |>
  split_term_column(varnames = categorical_vars) |>
  filter(variable %in% categorical_vars) |>
  distinct(variable, value) |>
  group_by(variable) |>
  summarise(levels = list(sort(unique(value))), .groups = "drop") |>
  deframe()

# ----- Step 4: Standardize coefs (implicit zeros + G-U) -----
std_1970 <- standardize_coefs(
  reg_data  = coefs_1970,
  adjust_by = adjust_by,
  coef_col  = "estimate",
  se_col    = "std.error"
) |>
  rename(coef_1970 = estimate, coef_1970_se = std.error)

std_2020 <- standardize_coefs(
  reg_data  = coefs_2020,
  adjust_by = adjust_by,
  coef_col  = "estimate",
  se_col    = "std.error"
) |>
  rename(coef_2020 = estimate, coef_2020_se = std.error)

# ----- Step 5: Prep props with year-suffixed column names -----
tidy_props <- function(props, year) {
  props |>
    select(term, estimate, std.error) |>
    rename(
      !!paste0("prop_", year)        := estimate,
      !!paste0("prop_", year, "_se") := std.error
    )
}

props_1970_tidy <- tidy_props(props_1970, 1970)
props_2020_tidy <- tidy_props(props_2020, 2020)

# ----- Step 6: Join everything into kob_input -----
kob_input <- std_1970 |>
  select(term, variable, value, coef_1970, coef_1970_se) |>
  full_join(
    std_2020 |> select(term, coef_2020, coef_2020_se),
    by = "term"
  ) |>
  left_join(props_1970_tidy, by = "term") |>
  left_join(props_2020_tidy, by = "term") |>
  arrange(variable, value)

# ----- Step 7: Save -----
saveRDS(kob_input, "bedroom-allocation/throughput/kob_input.rds")

message(sprintf("Wrote kob_input with %d rows.", nrow(kob_input)))
