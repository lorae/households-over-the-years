# process-person-level-ipums.R
#
# This script adds bucket columns to raw (person-level) data.
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")

devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums_cps.duckdb")
ipums_db <- tbl(con, "ipums")

# For data validation: count number of rows, to ensure none are dropped later
obs_count <- ipums_db |>
  summarise(count = n()) |>
  pull()


# ----- Step 2: Add columns ----- #

ipums_person <- ipums_db |>
  mutate(
    # Group people into age buckets
    # Note some years top code at age 90
    age_bucket = case_when(
      AGE < 18 ~ "17 or younger",
      AGE >= 18 & AGE < 30 ~ "18-29",
      AGE >= 30 & AGE < 50 ~ "30-49",
      AGE >= 50 & AGE < 65 ~ "50-65",
      AGE >= 65 ~ "65 and older"
    )
  )

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