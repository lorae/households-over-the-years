# compute-cf-crowding-2020.R
#
# Compute 2020 counterfactual crowding (persons per bedroom) from the
# predictions saved by predict-2020-bedrooms.R.
#
# Workflow:
#   1. Load predictions (hhid, NUMPREC, predicted bedrooms).
#   2. Pull only hhid + PERWT for all 2020 persons from raw IPUMS.
#   3. Left join predictions onto persons and drop persons whose HH isn't
#      in the predictions set (i.e., HHs with any missing regressor).
#   4. Compute per-person crowding = NUMPREC / predicted_bedrooms_rounded
#      (studios -> 1 bedroom), then PERWT-weighted mean.
#
# Inputs:
#   bedroom-allocation/throughput/hh-predictions-2020.rds
#   data/bedroom-allocation-kob-db/ipums.duckdb
#
# Outputs: prints weighted mean crowding to console.

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)

# ----- Step 1: Load predictions -----
predictions <- readRDS("bedroom-allocation/throughput/hh-predictions-2020.rds")

# ----- Step 2: Load 2020 person-level PERWT from raw IPUMS -----
ipums_con <- dbConnect(
  duckdb::duckdb(),
  "data/bedroom-allocation-kob-db/ipums.duckdb",
  read_only = TRUE
)

persons_2020 <- tbl(ipums_con, "ipums") |>
  filter(YEAR == 2022L, GQ %in% c(0, 1, 2)) |>
  mutate(hhid = paste0(SAMPLE, "_", SERIAL)) |>
  select(hhid, PERWT) |>
  collect()

dbDisconnect(ipums_con)

# ----- Step 3: Left join predictions; drop persons without prediction -----
scored <- persons_2020 |>
  left_join(predictions, by = "hhid") |>
  filter(!is.na(predicted_bedrooms_rounded)) |>
  mutate(
    # Studios (predicted 0) treated as 1 bedroom for crowding
    bedrooms_for_crowding = pmax(predicted_bedrooms_rounded, 1L),
    crowding              = NUMPREC / bedrooms_for_crowding
  )

# ----- Step 4: PERWT-weighted mean crowding -----
counterfactual_crowding <- with(scored, sum(crowding * PERWT) / sum(PERWT))

cat(sprintf(
  "2020 counterfactual crowding (PERWT-weighted, rounded predictions): %.3f persons per bedroom\n",
  counterfactual_crowding
))
cat(sprintf("Based on %d persons in %d HHs.\n",
            nrow(scored), n_distinct(scored$hhid)))
