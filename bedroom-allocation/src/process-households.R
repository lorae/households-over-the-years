# process-households.R
#
# Build a regression-ready household-level dataset for the bedroom-allocation
# sub-project. Pulls from the shared IPUMS DuckDB, filters to non-institutional
# householders who own or rent, and adds derived columns including the
# build-cohort mapping from BUILTYR2.
#
# Inputs:
# - data/five-decade-db/ipums.duckdb (table: ipums)
#
# Outputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)

library(dplyr)
library(dbplyr)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb", read_only = TRUE)

ipums <- tbl(con, "ipums")

# --- household-level, non-institutional, owner or renter ---

households <- ipums |>
  filter(
    GQ %in% c(0, 1, 2),
    PERNUM == 1,
    OWNERSHP %in% c(1, 2)
  ) |>
  mutate(
    decade = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    bedrooms_recode = case_when(
      is.na(BEDROOMS) | BEDROOMS == 0L ~ NA_integer_,  # NA or IPUMS N/A code
      TRUE                              ~ pmin(BEDROOMS - 1L, 5L)
    ),
    tenure = case_when(
      OWNERSHP == 1L ~ "owner",
      OWNERSHP == 2L ~ "renter"
    ),
    # First deal with BUILTYR2 variable: use over BUILTYR whenever available
    # (in 2000, there are both)
    build_cohort_2 = case_when(
      BUILTYR2 == 0 ~ NA_character_,
      BUILTYR2 == 1 ~ "1939 or earlier",
      BUILTYR2 == 2 ~ "1940 - 1949",
      BUILTYR2 == 3 ~ "1950 - 1959",
      BUILTYR2 == 4 ~ "1960 - 1969",
      BUILTYR2 == 5 ~ "1970 - 1979",
      BUILTYR2 == 6 ~ "1980 - 1989",
      BUILTYR2 %in% c(7,8) ~ "1990 - 1999",
      BUILTYR2 %in% c(9, 10, 11, 12, 13, 14) ~ "2000 - 2009",
      BUILTYR2 %in% c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24) ~ "2010 - 2019",
      BUILTYR2 %in% c(25, 26, 27, 28) ~ "2020 onward"
    ),
    build_literal_min = case_when(
      BUILTYR == 0 ~ NA_real_,
      BUILTYR == 1 ~ YEAR - 1, # As a judgement call, I'm going to make this 0-1
      # year category always occur in the previous decade. This is because it 
      # displays nicely on the cart and is more easily intrepretable. Unfortunately,
      # it is impossible to ascertain whether responses collected in a decade 
      # year (say, 2020) were built in that decade (2020-2029) or in the previous 
      # one.
      BUILTYR == 2 ~ YEAR - 5,
      BUILTYR == 3 ~ YEAR - 10,
      BUILTYR == 4 ~ YEAR - 20,
      BUILTYR == 5 ~ YEAR - 30,
      BUILTYR == 6 ~ YEAR - 40, # 1930 for 1970
      BUILTYR == 7 ~ YEAR - 50, # 1930 for 1980
      BUILTYR == 8 ~ YEAR - 60, # 1930 for 1990
      BUILTYR == 9 ~ YEAR - 61  # 1939 for 2000
    ),
    build_literal_max = case_when(
      BUILTYR == 0 ~ NA_real_,
      BUILTYR == 1 ~ YEAR -1,
      BUILTYR == 2 ~ YEAR - 2,
      BUILTYR == 3 ~ YEAR - 6,
      BUILTYR == 4 ~ YEAR - 11,
      BUILTYR == 5 ~ YEAR - 21,
      BUILTYR == 6 ~ YEAR - 31,
      BUILTYR == 7 ~ YEAR - 41,
      BUILTYR == 8 ~ YEAR - 51,
      BUILTYR == 9 ~ YEAR - 61
    ),
    # Map BUILTYR ranges into the same bins as build_cohort_2.
    # If min and max both fall in the same bin, assign. Otherwise error.
    build_cohort_1 = case_when(
      is.na(build_literal_min) ~ NA_character_,
      build_literal_max < 1940 ~ "1939 or earlier",
      build_literal_min >= 1940 & build_literal_max < 1950 ~ "1940 - 1949",
      build_literal_min >= 1950 & build_literal_max < 1960 ~ "1950 - 1959",
      build_literal_min >= 1960 & build_literal_max < 1970 ~ "1960 - 1969",
      build_literal_min >= 1970 & build_literal_max < 1980 ~ "1970 - 1979",
      build_literal_min >= 1980 & build_literal_max < 1990 ~ "1980 - 1989",
      build_literal_min >= 1990 & build_literal_max < 2000 ~ "1990 - 1999",
      build_literal_min >= 2000 & build_literal_max < 2010 ~ "2000 - 2009",
      build_literal_min >= 2010 & build_literal_max < 2020 ~ "2010 - 2019",
      build_literal_min >= 2020 ~ "2020 onward",
      .default = "ERROR: spans multiple bins"
    ),
    # Combined: prefer build_cohort_2 (BUILTYR2), fall back to build_cohort_1 (BUILTYR)
    build_cohort = case_when(
      !is.na(build_cohort_2) ~ build_cohort_2,
      .default = build_cohort_1
    )
  )

# --- write households table to throughput ---

households <- households |> collect()

dbDisconnect(con)

out_con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb"
)
dbWriteTable(out_con, "households", households, overwrite = TRUE)
dbDisconnect(out_con, shutdown = TRUE)
