# process-person-level-ipums.R
#
# This script adds bucket columns to raw (person-level) data.
# It reads data from the "ipums" table in `/five-decade-db/ipums.duckdb` and writes processed
# data to the "ipums-bucketed" table in `/five-decade-db/ipums-processed.duckdb`.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")

devtools::load_all("../demographr")

# ----- Step 0.5: Load helper data ----- #
inflators <- read_csv("reference/inflators-1970-2020.csv") |>
  distinct(YEAR, .keep_all = TRUE) |>
  mutate(YEAR = as.integer(YEAR))

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")
ipums_db <- tbl(con, "ipums")

# For data validation: count number of rows, to ensure none are dropped later
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()


# ----- Step 2: Add columns ----- #

# Make inflators available inside DuckDB
inflators_db <- copy_to(con, inflators, name = "inflators", temporary = TRUE, overwrite = TRUE)

ipums_person <- ipums_db |>
  mutate(
    # Top-code at 5, since 1940- 1970 has most restrictive top-code
    n_multifam = case_when(
      NFAMS == 0 ~ 0,
      NFAMS == 1 ~ 1,
      NFAMS == 2 ~ 2,
      NFAMS == 3 ~ 3,
      NFAMS == 4 ~ 4,
      NFAMS >= 5 ~ 5
    ),
    is_multifam = case_when(
      n_multifam == 0 ~ NA,
      n_multifam == 1 ~ FALSE,
      n_multifam >= 2 ~ TRUE
    ),
    # Top-code at 9, which is the top-code for 2006 and earlier (most restrictive)
    room = case_when(
      ROOMS < 9 ~ ROOMS,
      ROOMS >= 9 ~ 9
    ),
    # Recode integers and top-code at 56, which is the top-code for 2000 and earlier (most restrictive)
    bedroom = case_when(
      BEDROOMS == 0 ~ NA_integer_,
      BEDROOMS == 1 ~ 1, # efficiencies / studios: we classify as 1 bedroom
      BEDROOMS == 2 ~ 1,
      BEDROOMS == 3 ~ 2,
      BEDROOMS == 4 ~ 3,
      BEDROOMS == 5 ~ 4,
      BEDROOMS >= 6 ~ 5
    ),
    # Persons per bedroom
    ppbr = NUMPREC / bedroom,
    # Group people into age buckets
    # Note some years top code at age 90
    age_bucket = case_when(
      AGE < 18 ~ "17 or younger",
      AGE >= 18 & AGE < 30 ~ "18-29",
      AGE >= 30 & AGE < 50 ~ "30-49",
      AGE >= 50 & AGE < 65 ~ "50-65",
      AGE >= 65 ~ "65 and older"
    ),
    is_hispan = case_when(
      HISPAN == 9 ~ NA,
      HISPAN == 0 ~ FALSE,
      HISPAN == 1 ~ TRUE,
      HISPAN == 2 ~ TRUE,
      HISPAN == 3 ~ TRUE,
      HISPAN == 4 ~ TRUE
    ),
    race_bucket = case_when(
      RACE == 1 ~ "white",
      RACE == 2 ~ "black",
      RACE == 3 ~ "aian",
      RACE %in% c(4, 5, 6) ~ "aapi",
      RACE %in% c(8, 9) ~ "multi",
      RACE == 7 ~ "other"
    ),
    race_eth = case_when(
      is_hispan ~ "Hispanic", # All Hispanics labelled as "Hispanic" regardless of race
      race_bucket == "black" ~ "Black",
      race_bucket == "aapi" ~ "AAPI",
      race_bucket == "aian" ~ "AIAN",
      race_bucket == "multi" ~ "Multiracial",
      race_bucket == "white" ~ "White",
      race_bucket == "other" ~ "Other"
    ),
    tenure = case_when(
      OWNERSHP == 0 ~ NA_character_,
      OWNERSHP == 1 ~ "owner",
      OWNERSHP == 2 ~ "renter"
    ),
    birthplace = case_when(
      BPL <= 120 ~ "U.S.-born",
      BPL > 120 ~ "foreign-born"
    )
  ) |>
  left_join(inflators_db, by = "YEAR")

# ----- Step 3: Compute, save, close out the connection ----- #

# Create a new table to write processed columns to
compute(
  ipums_person,
  name = "ipums_person",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate no rows were dropped
validate_row_counts(
  db = tbl(con, "ipums_person"),
  expected_count = obs_count,
  step_description = "ipums_person db was created"
)

dbDisconnect(con)