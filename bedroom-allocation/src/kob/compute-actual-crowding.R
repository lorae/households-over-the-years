# compute-actual-crowding.R
#
# Compute actual crowding (persons per bedroom) for 1970 and 2020 from
# raw IPUMS person-level data. Sample: non-GQ persons in HHs with a
# valid BEDROOMS value. Broader than the KOB regression sample - includes
# persons in HHs missing regressor data (e.g., REGION 99).
#
# Studios (BEDROOMS = 1, i.e. bedrooms_recode = 0) treated as 1 bedroom
# for crowding purposes.
#
# Inputs: data/bedroom-allocation-kob-db/ipums.duckdb
# Outputs: prints weighted mean crowding per year to console.

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)

ipums_con <- dbConnect(
  duckdb::duckdb(),
  "data/bedroom-allocation-kob-db/ipums.duckdb",
  read_only = TRUE
)

compute_actual_crowding <- function(year_val) {
  tbl(ipums_con, "ipums") |>
    filter(
      YEAR == !!year_val,
      GQ %in% c(0L, 1L, 2L),           # non-GQ
      !is.na(BEDROOMS), BEDROOMS != 0L # valid BEDROOMS
    ) |>
    mutate(
      bedrooms_recode       = pmin(BEDROOMS - 1L, 5L),  # match HH pipeline recode
      bedrooms_for_crowding = pmax(bedrooms_recode, 1L), # studios -> 1
      crowding              = NUMPREC / bedrooms_for_crowding
    ) |>
    summarise(
      mean_crowding = sum(crowding * PERWT) / sum(PERWT),
      n_persons     = n()
    ) |>
    collect()
}

c_1970 <- compute_actual_crowding(1970L)
c_2020 <- compute_actual_crowding(2022L)

dbDisconnect(ipums_con)

cat(sprintf("1970 actual crowding (PERWT-weighted): %.3f persons per bedroom\n",
            c_1970$mean_crowding))
cat(sprintf("  Based on %d persons.\n", c_1970$n_persons))

cat(sprintf("2020 actual crowding (PERWT-weighted): %.3f persons per bedroom\n",
            c_2020$mean_crowding))
cat(sprintf("  Based on %d persons.\n", c_2020$n_persons))
