# usa-household-bedroom-crowding.R

# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

# ================================
# Year map
# ================================
acs_year_map <- tibble(
  YEAR = c(1970, 1980, 1990, 2000, 2012, 2022),
  YEAR_OUT = c(1970, 1980, 1990, 2000, 2010, 2020)
)

# ================================
# ACS: connect DB
# ================================
con_acs <- dbConnect(
  duckdb::duckdb(),
  "data/five-decade-db/ipums.duckdb"
)

ipums_person_acs <- tbl(con_acs, "ipums_person") |>
  mutate(in_gq = !GQ %in% c(0, 1, 2))

ipums_person_acs


# ================================
# Household ID + foreign-born household flag (DuckDB-safe)
# ================================

ipums_person_acs <- tbl(con_acs, "ipums_person") |>
  mutate(
    in_gq = !GQ %in% c(0, 1, 2),
    hhid  = paste0(SAMPLE, "_", SERIAL)
  ) |>
  group_by(hhid) |>
  mutate(
    hh_has_foreign_born = any(birthplace == "foreign-born")
  ) |>
  ungroup()

foreign_born <- crosstab_percent(
  data = ipums_person_acs,
  wt_col = "PERWT",
  group_by = c("YEAR", "hh_has_foreign_born"),
  percent_group_by = c("YEAR")
)

#  While only 10.2% of U.S. households included a foreign-born individual in
# 1970, this was true of 25.7% of households in 2020. 
foreign_born |> arrange(YEAR, hh_has_foreign_born)
