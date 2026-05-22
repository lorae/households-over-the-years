# build-kob-output.R
#
# Runs the KOB decomposition on kob_input (1970 base, 2020 compare), then
# validates the total against the observed mean-bedrooms gap computed
# directly from the microdata.
#
# Inputs:
# - bedroom-allocation/throughput/kob_input.rds
# - bedroom-allocation/throughput/kob-households.duckdb (for observed means)
#
# Outputs:
# - bedroom-allocation/throughput/kob_output.rds

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)

source("bedroom-allocation/src/kob/kob-function.R")

# ----- Step 1: Load kob_input -----
kob_input <- readRDS("bedroom-allocation/throughput/kob_input.rds")

# ----- Step 2: Compute observed weighted-mean bedrooms per year -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

# Match the regressions' row set exactly: svyglm's na.omit drops rows with
# NA in ANY regressor, so the observed-mean filter must mirror that.
observed_means <- tbl(con, "households") |>
  filter(
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
  group_by(decade) |>
  summarise(
    mean_bedrooms = sum(bedrooms_recode * HHWT) / sum(HHWT),
    .groups = "drop"
  ) |>
  collect()

dbDisconnect(con)

mean_1970 <- observed_means |> filter(decade == 1970) |> pull(mean_bedrooms)
mean_2020 <- observed_means |> filter(decade == 2020) |> pull(mean_bedrooms)

message(sprintf("Observed mean bedrooms 1970: %.4f", mean_1970))
message(sprintf("Observed mean bedrooms 2020: %.4f", mean_2020))
message(sprintf("Observed gap (2020 - 1970):   %.4f", mean_2020 - mean_1970))

# ----- Step 3: Run KOB decomposition -----
kob_output <- kob(kob_input, base_year = 1970, compare_year = 2020)

# ----- Step 4: Validate decomposition total against observed gap -----
# Tolerance loose enough to absorb float accumulation over ~25 sums, tight
# enough to catch any real bug.
kob_output_validate(
  kob_output,
  mean_base    = mean_1970,
  mean_compare = mean_2020,
  tol          = 1e-4
)

# ----- Step 5: Save -----
saveRDS(kob_output, "bedroom-allocation/throughput/kob_output.rds")

message(sprintf("Saved kob_output with %d rows.", nrow(kob_output)))
