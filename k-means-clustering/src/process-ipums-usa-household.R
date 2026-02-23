# process-household-level-ipums.R
#
# This script creates household-level aggregations from person-level data.
# It reads data from the "ipums" table in `/db/ipums.duckdb` and writes
# household-level data to the "ipums_household" table in the same database.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("dbplyr")
devtools::load_all("../demographr")

# ----- Step 1: Connect to the database ----- #
con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

# For data validation: count number of unique households
household_count <- ipums_person |>
  distinct(SERIAL, YEAR) |>
  summarise(count = n()) |>
  pull()

# ----- Step 2: Create household-level aggregations ----- #
ipums_household <- ipums_person |>
  group_by(SERIAL, YEAR) |>
  summarise(
    # Household size
    hh_size = n(),
    
    # NUMPREC (should match household size, for verification)
    NUMPREC = first(NUMPREC),
    
    # Unit characteristics
    room = first(room),
    bedroom = first(bedroom),
    
    # Number of families in household (take max since it's constant within household)
    n_multifam = max(n_multifam, na.rm = TRUE),
    is_multifam = max(is_multifam, na.rm = TRUE),
    GQ = first(GQ),
    HHWT = first(HHWT),
    
    # Household composition
    n_adults = sum(AGE >= 18, na.rm = TRUE),
    n_children = sum(AGE < 18, na.rm = TRUE),
    
    # Household relationships
    # We assume the HOH and their spouse take equal prevalence in the analysis.
    # PArents-in-law are counted as parents
    n_spouse = sum(RELATE == 2),
    n_child = sum(RELATE == 3),
    n_parent = sum(RELATE == 5 | RELATE == 6),
    n_grandchild = sum(RELATE == 9),
    n_other_rel = sum(RELATE == 4 | RELATE == 7 | RELATE == 8 | RELATE == 10),
    n_non_rel = sum(RELATE == 11 | RELATE == 12),
    
    .groups = "drop"
  )

# ----- Step 3: Compute, save, close out the connection ----- #

# Create a new table to write processed columns to
compute(
  ipums_household,
  name = "ipums_household",
  temporary = FALSE,
  overwrite = TRUE
)

# Validate no rows were dropped
validate_row_counts(
  db = tbl(con, "ipums_household"),
  expected_count = household_count,
  step_description = "ipums_household db was created"
)

dbDisconnect(con)
