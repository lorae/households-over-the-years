# usa-household-bedroom-crowding.R
# The purpose of this script is to aggregate average household size, number of 
# bedrooms, and crowding by decade.

# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_decade_usa <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 

bedroom_decade_usa <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "bedroom",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 

ppbr_decade_usa <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 


# ----- Step 0: CPS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums_cps.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_decade_cps <- crosstab_mean(
  data = ipums_person, # no GQ variable?
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = c("YEAR")
)

# ----- Combine ----- #
library(dplyr)
library(tidyr)
library(writexl)

# ---- Target years ----
years <- c(1970, 1980, 1990, 2000, 2010, 2020)

# ---- CPS ----
cps_obs <- hhsize_decade_cps |>
  filter(YEAR %in% years) |>
  select(YEAR, value = count) |>
  mutate(row = "CPS observations")

cps_hhsize <- hhsize_decade_cps |>
  filter(YEAR %in% years) |>
  select(YEAR, value = weighted_mean) |>
  mutate(row = "CPS household size")

# ---- ACS (map 2022 -> 2020) ----
acs_year_map <- tibble(
  YEAR = c(1970, 1980, 1990, 2000, 2012, 2022),
  YEAR_OUT = c(1970, 1980, 1990, 2000, 2010, 2020)
)

acs_obs <- hhsize_decade_usa |>
  left_join(acs_year_map, by = "YEAR") |>
  select(YEAR = YEAR_OUT, value = count) |>
  mutate(row = "ACS observations")

acs_hhsize <- hhsize_decade_usa |>
  left_join(acs_year_map, by = "YEAR") |>
  select(YEAR = YEAR_OUT, value = weighted_mean) |>
  mutate(row = "ACS household size")

acs_bedrooms <- bedroom_decade_usa |>
  left_join(acs_year_map, by = "YEAR") |>
  select(YEAR = YEAR_OUT, value = weighted_mean) |>
  mutate(row = "ACS bedrooms")

acs_ppbr <- ppbr_decade_usa |>
  left_join(acs_year_map, by = "YEAR") |>
  select(YEAR = YEAR_OUT, value = weighted_mean) |>
  mutate(row = "ACS persons per bedroom")

# ---- Combine + reshape ----
final_table <- bind_rows(
  cps_obs,
  acs_obs,
  cps_hhsize,
  acs_hhsize,
  acs_bedrooms,
  acs_ppbr
) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  ) |>
  select(
    row,
    `1970`, `1980`, `1990`, `2000`, `2010`, `2020`
  ) |>
  arrange(factor(
    row,
    levels = c(
      "CPS observations",
      "ACS observations",
      "CPS household size",
      "ACS household size",
      "ACS bedrooms",
      "ACS persons per bedroom"
    )
  ))


# ---- Write Excel ----
dir.create("output/five-decade-tables", recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  final_table,
  path = "output/five-decade-tables/usa_household_size_bedrooms_crowding_by_decade_raw.xlsx"
)

