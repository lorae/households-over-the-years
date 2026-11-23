# generate-crowding-cost-state-tenure-decade.R
#
# Aggregates by state x decade x tenure (owner/renter):
#   - Mean persons per bedroom (ppbr), PERWT-weighted (person-level)
#   - Mean annual housing cost in 2020$, HHWT-weighted (household-level via PERNUM == 1)
#       * owners:  owncost_2020 * 12       (owncost_2020 is precomputed in ipums_person)
#       * renters: RENT * inflator_2020 * 12   (inflated inline; RENT not precomputed)
#
# Inputs:
# - data/five-decade-db/ipums.duckdb (table: ipums_person)
# - ../demographr (sibling package)
# - five-decade-aggregates/src/helpers/setup.R
#
# Outputs:
# - five-decade-aggregates/output/raw/ppbr_state_tenure_decade.csv
# - five-decade-aggregates/output/raw/crowded_state_tenure_decade.csv
# - five-decade-aggregates/output/raw/housing_cost_state_tenure_decade.csv

library(dplyr)
library(duckdb)
library(dbplyr)
library(readr)

devtools::load_all("../demographr")
source("five-decade-aggregates/src/helpers/setup.R")

# ================================
# Helpers: STATEFIP -> state name
# (copied from generate-crowding-state-decade.R)
# ================================

state_lookup <- function(include_pr = FALSE, include_groups = FALSE) {
  x <- tibble::tribble(
    ~STATEFIP, ~state_name,
    1L,  "Alabama",
    2L,  "Alaska",
    4L,  "Arizona",
    5L,  "Arkansas",
    6L,  "California",
    8L,  "Colorado",
    9L,  "Connecticut",
    10L, "Delaware",
    11L, "District of Columbia",
    12L, "Florida",
    13L, "Georgia",
    15L, "Hawaii",
    16L, "Idaho",
    17L, "Illinois",
    18L, "Indiana",
    19L, "Iowa",
    20L, "Kansas",
    21L, "Kentucky",
    22L, "Louisiana",
    23L, "Maine",
    24L, "Maryland",
    25L, "Massachusetts",
    26L, "Michigan",
    27L, "Minnesota",
    28L, "Mississippi",
    29L, "Missouri",
    30L, "Montana",
    31L, "Nebraska",
    32L, "Nevada",
    33L, "New Hampshire",
    34L, "New Jersey",
    35L, "New Mexico",
    36L, "New York",
    37L, "North Carolina",
    38L, "North Dakota",
    39L, "Ohio",
    40L, "Oklahoma",
    41L, "Oregon",
    42L, "Pennsylvania",
    44L, "Rhode Island",
    45L, "South Carolina",
    46L, "South Dakota",
    47L, "Tennessee",
    48L, "Texas",
    49L, "Utah",
    50L, "Vermont",
    51L, "Virginia",
    53L, "Washington",
    54L, "West Virginia",
    55L, "Wisconsin",
    56L, "Wyoming"
  )

  if (include_pr) {
    x <- bind_rows(x, tibble(STATEFIP = 72L, state_name = "Puerto Rico"))
  }

  if (include_groups) {
    x <- bind_rows(
      x,
      tibble::tribble(
        ~STATEFIP, ~state_name,
        61L, "Maine-New Hampshire-Vermont (group)",
        62L, "Massachusetts-Rhode Island (group)",
        63L, "MN-IA-MO-KS-NE-SD-ND (group)",
        64L, "Maryland-Delaware (group)",
        65L, "Montana-Idaho-Wyoming (group)",
        66L, "Utah-Nevada (group)",
        67L, "Arizona-New Mexico (group)",
        68L, "Alaska-Hawaii (group)",
        97L, "Overseas Military Installations",
        99L, "State not identified"
      )
    )
  }

  x
}

add_state_names <- function(df, statefip_col = "STATEFIP",
                            include_pr = FALSE, include_groups = FALSE) {
  lookup <- state_lookup(include_pr = include_pr, include_groups = include_groups)

  df |>
    mutate(STATEFIP = as.integer(.data[[statefip_col]])) |>
    left_join(lookup, by = "STATEFIP")
}

# ----- Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

out_dir <- "five-decade-aggregates/output/raw"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ================================
# Mean persons per bedroom (ppbr) by state x year x tenure
# (PERWT, person-level; matches generate-crowding-state-decade.R conventions)
# ================================

ppbr_base <- tbl(con, "ipums_person") |>
  filter(GQ %in% c(0L, 1L, 2L), OWNERSHP %in% c(1L, 2L)) |>
  mutate(tenure = if_else(OWNERSHP == 1L, "owner", "renter")) |>
  filter(PERNUM == 1L) # get household-level data

ppbr_state_tenure_decade <- crosstab_mean(
  data = ppbr_base,
  value = "ppbr",
  wt_col = "HHWT",
  group_by = c("STATEFIP", "YEAR", "tenure")
) |>
  collect() |>
  clean_years() |>
  rename(mean_ppbr = weighted_mean) |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, tenure, state_name)

write_csv(
  ppbr_state_tenure_decade,
  file.path(out_dir, "ppbr_state_tenure_decade.csv")
)

# ================================
# % of households crowded (ppbr > 2) by state x year x tenure
# HHWT, household-level (PERNUM == 1)
# ================================

crowded_base <- tbl(con, "ipums_person") |>
  filter(GQ %in% c(0L, 1L, 2L), OWNERSHP %in% c(1L, 2L), PERNUM == 1L) |>
  mutate(
    tenure  = if_else(OWNERSHP == 1L, "owner", "renter"),
    crowded = ppbr > 2
  )

crowded_state_tenure_decade <- crosstab_percent(
  data = crowded_base,
  wt_col = "HHWT",
  group_by = c("crowded", "STATEFIP", "YEAR", "tenure"),
  percent_group_by = c("STATEFIP", "YEAR", "tenure")
) |>
  collect() |>
  filter(crowded) |>
  select(-crowded) |>
  clean_years() |>
  rename(pct_crowded = percent) |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, tenure, state_name)

write_csv(
  crowded_state_tenure_decade,
  file.path(out_dir, "crowded_state_tenure_decade.csv")
)

# ================================
# Mean annual housing cost (2020$) by state x year x tenure
# HHWT, household-level (PERNUM == 1)
# ================================

owner_cost_input <- tbl(con, "ipums_person") |>
  filter(
    GQ %in% c(0L, 1L, 2L),
    PERNUM == 1L,
    OWNERSHP == 1L,
    !is.na(owncost_2020), owncost_2020 > 0
  ) |>
  mutate(annual_cost_2020 = owncost_2020 * 12)

owner_cost_state_decade <- crosstab_mean(
  data = owner_cost_input,
  value = "annual_cost_2020",
  wt_col = "HHWT",
  group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  clean_years() |>
  mutate(tenure = "owner")

renter_cost_input <- tbl(con, "ipums_person") |>
  filter(
    GQ %in% c(0L, 1L, 2L),
    PERNUM == 1L,
    OWNERSHP == 2L,
    !is.na(RENT), RENT != 9999L, RENT > 0L
  ) |>
  mutate(annual_cost_2020 = RENT * inflator_2020 * 12)

renter_cost_state_decade <- crosstab_mean(
  data = renter_cost_input,
  value = "annual_cost_2020",
  wt_col = "HHWT",
  group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  clean_years() |>
  mutate(tenure = "renter")

housing_cost_state_tenure_decade <- bind_rows(
  owner_cost_state_decade,
  renter_cost_state_decade
) |>
  rename(mean_annual_cost_2020 = weighted_mean) |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, tenure, state_name)

write_csv(
  housing_cost_state_tenure_decade,
  file.path(out_dir, "housing_cost_state_tenure_decade.csv")
)

dbDisconnect(con)
