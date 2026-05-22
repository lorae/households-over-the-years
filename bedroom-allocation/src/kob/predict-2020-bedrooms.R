# predict-2020-bedrooms.R
#
# Apply 1970 regression coefficients to 2020 household data to produce
# counterfactual bedroom predictions. Saved with hhid and NUMPREC for
# downstream use (e.g., crowding analyses).
#
# Per-HH fields saved:
#   - hhid
#   - NUMPREC (observed household size — persons in the HH)
#   - predicted_bedrooms_continuous: raw X %*% beta_1970
#   - predicted_bedrooms_rounded:    nearest integer, clamped to [0, 5]
#     (matches bedrooms_recode range: 0 = studio, 5 = 5+ topcode)
#
# Sample: regression-filtered 2020 HHs (all regressors non-NA). About 5%
# of 2020 HHs are dropped for missing regressor values; acceptable.
#
# Inputs:
#   bedroom-allocation/throughput/regressions/1970.rds
#   bedroom-allocation/throughput/kob-households.duckdb
#
# Outputs:
#   bedroom-allocation/throughput/hh-predictions-2020.rds

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)
library(tibble)

# ----- Step 1: Load 1970 regression coefficients -----
reg_1970 <- readRDS("bedroom-allocation/throughput/regressions/1970.rds")
coef_vec <- setNames(reg_1970$coefs$estimate, reg_1970$coefs$term)

# ----- Step 2: Load 2020 HH regressor data + NUMPREC -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

hhs_2020 <- tbl(con, "households") |>
  filter(
    decade == 2020L,
    !is.na(bedrooms_recode),
    !is.na(hoh_age), !is.na(hoh_race_eth), !is.na(hoh_educ_bucket),
    !is.na(hoh_us_born), !is.na(hoh_sex), !is.na(tenure),
    !is.na(region4), !is.na(n_children_under_18), !is.na(n_adults),
    !is.na(hhincome_2020_harmonized)
  ) |>
  select(hhid, NUMPREC, bedrooms_recode,
         hoh_age, hoh_race_eth, hoh_educ_bucket, hoh_us_born, hoh_sex,
         tenure, region4, n_children_under_18, n_adults,
         hhincome_2020_harmonized) |>
  collect()

dbDisconnect(con)

message(sprintf("Loaded %d 2020 HHs (regression sample).", nrow(hhs_2020)))

# ----- Step 3: Compute predictions -----
formula <- bedrooms_recode ~
  hoh_age + hoh_race_eth + hoh_educ_bucket + hoh_us_born +
  hoh_sex + tenure + region4 +
  n_children_under_18 + n_adults + hhincome_2020_harmonized

X <- model.matrix(formula, data = hhs_2020)
missing_cols <- setdiff(colnames(X), names(coef_vec))
if (length(missing_cols) > 0) {
  stop("X columns missing from 1970 coefs: ", paste(missing_cols, collapse = ", "))
}
coef_aligned <- coef_vec[colnames(X)]

predicted_continuous <- as.numeric(X %*% coef_aligned)
predicted_rounded    <- pmin(pmax(round(predicted_continuous), 0L), 5L)

# ----- Step 4: Save -----
predictions <- tibble(
  hhid                          = hhs_2020$hhid,
  NUMPREC                       = hhs_2020$NUMPREC,
  predicted_bedrooms_continuous = predicted_continuous,
  predicted_bedrooms_rounded    = predicted_rounded
)

output_path <- "bedroom-allocation/throughput/hh-predictions-2020.rds"
saveRDS(predictions, output_path)

message(sprintf("Saved %d predictions to %s", nrow(predictions), output_path))

# Quick sanity summary
cat("\nContinuous prediction summary:\n")
print(summary(predictions$predicted_bedrooms_continuous))
cat("\nRounded prediction distribution:\n")
print(table(predictions$predicted_bedrooms_rounded))
