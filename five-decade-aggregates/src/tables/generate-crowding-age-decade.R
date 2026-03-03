# generate-crowding-age-decade.R
#
# Computes % crowded by age bucket and decade. ACS data only.
#
# Inputs:
# - data/five-decade-db/ipums.duckdb (table: ipums_person)
# - ../demographr (sibling package)
# - five-decade-aggregates/src/helpers/setup.R
#
# Outputs:
# - five-decade-aggregates/output/raw/crowded_age.csv
#
#
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("writexl")

devtools::load_all("../demographr")
source("five-decade-aggregates/src/helpers/setup.R")

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ================================
# Raw crosstabs
# ================================

# ====================
# Overall
# ====================

crowded_age_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "age_bucket", "YEAR"),
  percent_group_by = c("age_bucket", "YEAR")
)

age_levels <- c(
  "17 or younger",
  "18-29",
  "30-49",
  "50-65",
  "65 and older"
)

crowded_age <- crowded_age_decade_usa |>
  filter(crowded) |>
  select(-crowded) |>
  clean_years() |>
  mutate(age_bucket = factor(age_bucket, levels = age_levels)) |>
  rename(percent_crowded = percent) |>
  arrange(YEAR, age_bucket)

readr::write_csv(
  crowded_age,
  "five-decade-aggregates/output/raw/crowded_age.csv"
)
