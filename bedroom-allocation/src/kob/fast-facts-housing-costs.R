# fast-facts-housing-costs.R
#
# Three quick stats for the "Fast Facts" text:
#   1. Owner:   OWNCOST*12 / HHINCOME mean + median, per year
#   2. Renter:  mean HHINCOME (2020$) per year, + % change 1970 -> 2020
#   3. Renter:  mean annual RENT (2020$) per year, + % change 1970 -> 2020
#
# Uses the five-decade-aggregates pipeline's `ipums_person` derived table
# instead of raw `ipums`, because:
#   - Raw HHINCOME is NA for 1970 (us1970c). hhincome_2020_harmonized IS
#     populated for 1970 (summed from per-person INCTOT).
#   - owncost_2020 / hhincome_2020_harmonized in ipums_person are already
#     inflation-adjusted to 2020$.
#
# Caveats:
#   - OWNCOST is NA for 1970 and 2000 in the source samples (us1970c,
#     us2000g). Owner stat is shown for years where it's available
#     (1980, 1990, 2012, 2022) -- NOT 1970.
#   - Owner stat is for ALL owners, NOT restricted to mortgage-holders
#     (MORTGAGE / MORTAMT not in extract). The 20.9% published figure for
#     2020 is mortgage-holders-only; our number will differ.
#
# Inputs: data/five-decade-db/ipums.duckdb (needs ipums_person table, which
#         is built by five-decade-aggregates/src/process-ipums-usa-person-*.R)

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)

con <- dbConnect(
  duckdb::duckdb(),
  "data/five-decade-db/ipums.duckdb",
  read_only = TRUE
)

# ----- Owner cost-to-income ratio -----
# Both numerator and denominator are in the same currency (2020$), so the
# ratio equals the within-year (OWNCOST*12 / HHINCOME) in any year.
owner_data <- tbl(con, "ipums_person") |>
  filter(
    GQ %in% c(0L, 1L, 2L),
    PERNUM == 1L,
    OWNERSHP == 1L,
    !is.na(owncost_2020), owncost_2020 > 0,
    !is.na(hhincome_2020_harmonized), hhincome_2020_harmonized > 0
  ) |>
  mutate(
    cost_to_income_ratio = (owncost_2020 * 12) / hhincome_2020_harmonized,
    annual_owncost_2020  = owncost_2020 * 12
  ) |>
  select(YEAR, HHWT, cost_to_income_ratio, annual_owncost_2020, hhincome_2020_harmonized) |>
  collect()

owner_stats <- owner_data |>
  group_by(YEAR) |>
  summarise(
    mean_ratio               = sum(cost_to_income_ratio * HHWT) / sum(HHWT),
    mean_hhincome_2020       = sum(hhincome_2020_harmonized * HHWT) / sum(HHWT),
    mean_annual_owncost_2020 = sum(annual_owncost_2020 * HHWT) / sum(HHWT),
    n_hh                     = n(),
    .groups = "drop"
  ) |>
  arrange(YEAR)

# ----- Renter HHINCOME (2020$) and RENT annual (2020$) -----
# hhincome_2020_harmonized is already in 2020$; need to inflate RENT.
# Inflator is embedded in ipums_person (copied from the inflators CSV by
# the five-decade process script), so it's already in each row.
renter_stats <- tbl(con, "ipums_person") |>
  filter(
    GQ %in% c(0L, 1L, 2L),
    PERNUM == 1L,
    OWNERSHP == 2L,
    !is.na(hhincome_2020_harmonized),
    !is.na(RENT), RENT != 9999L, RENT > 0L,
    !is.na(inflator_2020)
  ) |>
  mutate(annual_rent_2020 = RENT * 12 * inflator_2020) |>
  group_by(YEAR) |>
  summarise(
    mean_hhincome_2020    = sum(hhincome_2020_harmonized * HHWT) / sum(HHWT),
    mean_annual_rent_2020 = sum(annual_rent_2020 * HHWT) / sum(HHWT),
    n_hh                  = n(),
    .groups = "drop"
  ) |>
  collect() |>
  arrange(YEAR)

dbDisconnect(con)

cat("=== Owner cost-to-income ratio (ALL owners; 1970 and 2000 unavailable) ===\n")
print(owner_stats)

cat("\n=== Renter HHINCOME (2020$) and annual RENT (2020$) ===\n")
print(renter_stats)

# Percent changes 1980 -> 2020 for owners (1970 unavailable)
o80 <- owner_stats  |> filter(YEAR == 1980L)
o20 <- owner_stats  |> filter(YEAR == 2022L)
if (nrow(o80) == 1 && nrow(o20) == 1) {
  pct_chg <- function(new, old) (new - old) / old * 100
  cat(sprintf(
    "\nOwner real HHINCOME change 1980 -> 2020: %+.1f%%\n",
    pct_chg(o20$mean_hhincome_2020, o80$mean_hhincome_2020)
  ))
  cat(sprintf(
    "Owner real OWNCOST change 1980 -> 2020:  %+.1f%%\n",
    pct_chg(o20$mean_annual_owncost_2020, o80$mean_annual_owncost_2020)
  ))
}

# Percent changes 1970 -> 2020 for renters
r70 <- renter_stats |> filter(YEAR == 1970L)
r20 <- renter_stats |> filter(YEAR == 2022L)
if (nrow(r70) == 1 && nrow(r20) == 1) {
  pct_chg <- function(new, old) (new - old) / old * 100
  cat(sprintf(
    "\nRenter real HHINCOME change 1970 -> 2020: %+.1f%%\n",
    pct_chg(r20$mean_hhincome_2020, r70$mean_hhincome_2020)
  ))
  cat(sprintf(
    "Renter real RENT change 1970 -> 2020:     %+.1f%%\n",
    pct_chg(r20$mean_annual_rent_2020, r70$mean_annual_rent_2020)
  ))
}
