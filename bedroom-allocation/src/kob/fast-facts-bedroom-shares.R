# fast-facts-bedroom-shares.R
#
# Bedroom-share stats for the Fast Facts text. All stats are household-
# level: the households table has one row per HH (PERNUM == 1 filter baked
# in by process-households.R), weighted by HHWT.
#
# Uses crosstab_percent from demographr.
#
# Inputs:  bedroom-allocation/throughput/households.duckdb
# Outputs: prints three tables to console.

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)

devtools::load_all("../demographr")

# Print wide so all columns show.
options(tibble.width = Inf)

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb",
  read_only = TRUE
)

# ----- 1. % of renter and owner HHs with <= 2 bedrooms, 1970 vs 2020 -----
# bedrooms_recode: 0=studio, 1=1BR, 2=2BR, 3=3BR, 4=4BR, 5=5+BR
small_units <- tbl(con, "households") |>
  filter(
    decade %in% c(1970L, 2020L),
    !is.na(bedrooms_recode),
    !is.na(tenure)
  ) |>
  mutate(small_unit = bedrooms_recode <= 2L) |>
  crosstab_percent(
    wt_col           = "HHWT",
    group_by         = c("decade", "tenure", "small_unit"),
    percent_group_by = c("decade", "tenure")
  ) |>
  collect() |>
  filter(small_unit) |>
  arrange(decade, tenure)

cat("% of HHs with 2 bedrooms or fewer, by decade and tenure:\n")
print(small_units)

# ----- 2. % of HHs in 4+ bedroom homes (context for 'marked increase') -----
large_units <- tbl(con, "households") |>
  filter(
    decade %in% c(1970L, 2020L),
    !is.na(bedrooms_recode),
    !is.na(tenure)
  ) |>
  mutate(large_unit = bedrooms_recode >= 4L) |>
  crosstab_percent(
    wt_col           = "HHWT",
    group_by         = c("decade", "tenure", "large_unit"),
    percent_group_by = c("decade", "tenure")
  ) |>
  collect() |>
  filter(large_unit) |>
  arrange(decade, tenure)

cat("\n% of HHs with 4+ bedrooms, by decade and tenure:\n")
print(large_units)

# ----- 3. % of 2020 HHs in units built since 1970 -----
post_1970_cohorts <- c(
  "1970 - 1979", "1980 - 1989", "1990 - 1999",
  "2000 - 2009", "2010 - 2019", "2020 onward"
)

built_since_1970 <- tbl(con, "households") |>
  filter(
    decade == 2020L,
    !is.na(build_cohort),
    !is.na(tenure)
  ) |>
  mutate(built_since_1970 = build_cohort %in% post_1970_cohorts) |>
  crosstab_percent(
    wt_col           = "HHWT",
    group_by         = c("tenure", "built_since_1970"),
    percent_group_by = "tenure"
  ) |>
  collect() |>
  filter(built_since_1970) |>
  arrange(tenure)

cat("\n% of 2020 HHs in units built 1970 or later, by tenure:\n")
print(built_since_1970)

dbDisconnect(con)
