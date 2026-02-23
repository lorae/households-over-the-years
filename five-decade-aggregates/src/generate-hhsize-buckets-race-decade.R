# hhsize-buckets-race-decade.csv

# ----- Step 0: Config ----- #
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



# ----- Step 2: Compute table ----- #
hhsize_topcode <- 8

hhsize_decade_race_groups <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("NUMPREC", "race_eth", "YEAR"),
  percent_group_by = c("race_eth", "YEAR")
) 

hhsize_decade_all_groups <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("NUMPREC", "YEAR"),
  percent_group_by = c("YEAR")
) |>
  mutate(race_eth = "All")

hhsize_decade_race <- bind_rows(
  hhsize_decade_race_groups,
  hhsize_decade_all_groups
) |>
  # Topcode household size
  mutate(
    NUMPREC = if_else(NUMPREC >= hhsize_topcode,
                      hhsize_topcode,
                      NUMPREC)
  ) |>
  group_by(race_eth, YEAR, NUMPREC) |>
  summarise(
    percent = sum(percent),
    count = sum(count),
    .groups = "drop"
  ) |>
  # Relabel ACS years using midpoint
  clean_years() |>
  mutate(
    race_eth = factor(race_eth,
                      levels = c(race_levels, "All"))
  ) |>
  arrange(YEAR, race_eth, NUMPREC)

write_csv(
  hhsize_decade_race,
  "five-decade-aggregates/output/raw/hhsize-buckets-race-decade.csv"
)
