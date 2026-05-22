# process-kob-households.R
#
# Build a regression-ready household-level dataset for the bedroom-allocation
# KOB decomposition. Pulls from the KOB-specific IPUMS DuckDB (1970 + 2020
# only), filters to non-institutional householders who own or rent, and adds
# HOH-level derived columns + HH-composition counts + harmonized HH income.
#
# Architecture:
#   - Block 2 runs a person-level aggregation (all non-GQ persons) per hhid
#     to produce n_children_under_18, n_adults, hhincome_2020_harmonized.
#     INCTOT harmonization is verbatim-ported from
#     five-decade-aggregates/src/process-ipums-usa-person-1970-2020.R.
#   - Block 3 builds the HOH-row table with all hoh_* demographic cols.
#   - Block 4 joins aggregates to HOH by hhid.
#
# Inputs:
# - data/bedroom-allocation-kob-db/ipums.duckdb (table: ipums)
# - five-decade-aggregates/reference/inflators-1970-2020.csv
#
# Outputs:
# - bedroom-allocation/throughput/kob-households.duckdb (table: households)

library(dplyr)
library(dbplyr)
library(duckdb)
library(readr)

# ----- Block 1: inflators CSV -> DuckDB temp table ----- #

inflators <- read_csv(
  "five-decade-aggregates/reference/inflators-1970-2020.csv",
  show_col_types = FALSE
) |>
  distinct(YEAR, .keep_all = TRUE) |>
  mutate(YEAR = as.integer(YEAR))

# Open as writable because copy_to(temporary = TRUE) needs write access to
# the temp schema. Nothing is written to the permanent schema.
con <- dbConnect(
  duckdb::duckdb(),
  "data/bedroom-allocation-kob-db/ipums.duckdb",
  read_only = FALSE
)

ipums        <- tbl(con, "ipums")
inflators_db <- copy_to(con, inflators, name = "inflators", temporary = TRUE, overwrite = TRUE)

# ----- Block 2: person-level -> HH aggregates ----- #
# INCTOT harmonization verbatim from
# five-decade-aggregates/src/process-ipums-usa-person-1970-2020.R:134-155:
#   - sentinel codes -> NA or 0
#   - inflate to 2020 dollars via inflator CSV
#   - apply universal $260K / -$16K caps

hh_aggregates <- ipums |>
  filter(GQ %in% c(0, 1, 2)) |>
  left_join(inflators_db, by = "YEAR") |>
  mutate(
    hhid = paste0(SAMPLE, "_", SERIAL),
    inctot_2020 = case_when(
      INCTOT == -9995 & YEAR == 1980 ~ -9900,
      INCTOT == 0 ~ 0,
      INCTOT == 1 ~ 0,
      INCTOT == 9999999 ~ NA_real_,
      INCTOT == 9999998 ~ NA_real_,
      TRUE ~ INCTOT * inflator_2020
    ),
    inctot_2020_harmonized = case_when(
      inctot_2020 >= 260000 ~ 260000,
      inctot_2020 <= -16000 ~ -16000,
      TRUE ~ inctot_2020
    )
  ) |>
  group_by(hhid) |>
  summarise(
    n_children_under_18 = sum(if_else(AGE < 18L,  1L, 0L), na.rm = TRUE),
    n_adults            = sum(if_else(AGE >= 18L, 1L, 0L), na.rm = TRUE),
    n_income_nonNA      = sum(if_else(!is.na(inctot_2020_harmonized), 1L, 0L), na.rm = TRUE),
    hhincome_sum        = sum(inctot_2020_harmonized, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Five-decade-aggregates NA handling: if every person in the HH has NA
    # income, HH income is NA. Otherwise it's the sum of non-NA values.
    hhincome_2020_harmonized = if_else(
      n_income_nonNA == 0, NA_real_, hhincome_sum
    )
  ) |>
  select(hhid, n_children_under_18, n_adults, hhincome_2020_harmonized)

# ----- Block 3: HOH rows with demographic derivations ----- #
# non-institutional householders who own or rent.

households <- ipums |>
  filter(
    GQ %in% c(0, 1, 2),
    PERNUM == 1,
    OWNERSHP %in% c(1, 2)
  ) |>
  mutate(
    hhid = paste0(SAMPLE, "_", SERIAL),
    decade = case_when(
      YEAR == 1970L ~ 1970L,
      YEAR == 2022L ~ 2020L
    ),
    bedrooms_recode = case_when(
      is.na(BEDROOMS) | BEDROOMS == 0L ~ NA_integer_,  # NA or IPUMS N/A code
      TRUE                              ~ pmin(BEDROOMS - 1L, 5L)
    ),
    tenure = case_when(
      OWNERSHP == 1L ~ "owner",
      OWNERSHP == 2L ~ "renter"
    ),
    # Consolidate IPUMS REGION (9 divisions + "mixed" codes 13/23/34/43 +
    # "not identified" 99) into 4 Census super-regions. 1970 Form 1 Metro
    # pre-2005 PUMAs can span division boundaries; 2020 has only the clean
    # division codes. Super-regions align across years. REGION == 99 maps
    # to NA (true multi-region metros, ~4% of 1970) and gets dropped
    # downstream by the regression's na.omit.
    region4 = case_when(
      REGION %in% c(11L, 12L, 13L)      ~ "northeast",
      REGION %in% c(21L, 22L, 23L)      ~ "midwest",
      REGION %in% c(31L, 32L, 33L, 34L) ~ "south",
      REGION %in% c(41L, 42L, 43L)      ~ "west",
      TRUE                               ~ NA_character_
    ),
    # HOH race/ethnicity. Hispanic supersedes race. Multiracial collapsed
    # into Other because 1970 IPUMS RACE has no multiracial codes.
    hoh_is_hispan = case_when(
      HISPAN == 9 ~ NA,
      HISPAN == 0 ~ FALSE,
      HISPAN %in% c(1, 2, 3, 4) ~ TRUE
    ),
    hoh_race_bucket = case_when(
      RACE == 1 ~ "white",
      RACE == 2 ~ "black",
      RACE == 3 ~ "aian",
      RACE %in% c(4, 5, 6) ~ "aapi",
      RACE %in% c(7, 8, 9) ~ "other"
    ),
    hoh_race_eth = case_when(
      hoh_is_hispan ~ "Hispanic",
      hoh_race_bucket == "black" ~ "Black",
      hoh_race_bucket == "aapi" ~ "AAPI",
      hoh_race_bucket == "aian" ~ "AIAN",
      hoh_race_bucket == "white" ~ "White",
      hoh_race_bucket == "other" ~ "Other"
    ),
    # HOH nativity. BPL 001-120 = U.S. or U.S. outlying; 150+ = foreign.
    hoh_us_born = BPL <= 120,
    # HOH sex
    hoh_sex = if_else(SEX == 1L, "male", "female"),
    # HOH age, continuous. Top-coded at 95 (2020 ACS cap) for cross-year
    # comparability; 1970 tops at 99.
    hoh_age = pmin(AGE, 95L),
    # HOH education, 3-bucket scheme matching
    # immigrant-households/src/scripts/hhsize-regression-over-time.R:62-71.
    # EDUC 0-6 covers everything up through HS diploma;
    # EDUC 7-9 is some college short of a 4-year degree;
    # EDUC 10-11 is 4-year college or more.
    hoh_educ_bucket = case_when(
      EDUC >= 0  & EDUC <= 6  ~ "hs_or_less",
      EDUC >= 7  & EDUC <= 9  ~ "some_college",
      EDUC >= 10 & EDUC <= 11 ~ "college_4yr+",
      TRUE                    ~ NA_character_
    )
  )

# ----- Block 4: join aggregates to HOH rows, collect ----- #

households <- households |>
  left_join(hh_aggregates, by = "hhid") |>
  collect()

dbDisconnect(con)

# ----- Block 5: write throughput ----- #

out_con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb"
)
dbWriteTable(out_con, "households", households, overwrite = TRUE)
dbDisconnect(out_con, shutdown = TRUE)
