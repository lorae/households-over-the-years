# process-ipums-cps-person-1970-2020.R
#
# Adds derived columns to the raw IPUMS CPS person-level data (age buckets,
# race/ethnicity).
# Writes the result as a new table in the same database.
#
# Inputs:
# - data/five-decade-db/ipums_cps.duckdb (table: ipums)
# - ../demographr (sibling package)
#
# Outputs:
# - data/five-decade-db/ipums_cps.duckdb (table: ipums_person)
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
    ),
    # Race/ethnicity: Hispanic overrides race
    is_hispan = case_when(
      HISPAN == 902 ~ NA,
      HISPAN == 901 ~ NA,
      HISPAN >= 100 & HISPAN <= 612 ~ TRUE,
      HISPAN == 0 ~ FALSE
      # NOTE: unmatched cases (e.g. 1970 where no hispanic data was collected)
      # will implicitly become NA
    ),
    race_bucket = case_when(
      RACE == 999 ~ NA_character_,
      RACE == 100 ~ "white",
      RACE == 200 ~ "black",
      RACE == 300 ~ "aian",
      RACE %in% c(650, 651, 652) ~ "aapi",
      RACE == 700 ~ "other",
      RACE >= 801 & RACE <= 830 ~ "multi",
      TRUE ~ NA_character_ # TODO: make sure this case never occurs. If it does, there is
      # a data / encoding issue because the above should encode for all cases
    ),
    race_eth = case_when(
      is_hispan ~ "Hispanic",
      race_bucket == "black" ~ "Black",
      race_bucket == "aapi" ~ "AAPI",
      race_bucket == "aian" ~ "AIAN",
      race_bucket == "multi" ~ "Multiracial",
      race_bucket == "white" ~ "White",
      race_bucket == "other" ~ "Other"
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