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
ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

crowded_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("crowded", "YEAR"),
  percent_group_by = ("YEAR")
) 

crowded_race_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("race_eth", "crowded", "YEAR"),
  percent_group_by = ("YEAR")
) |>
  filter(crowded) |>

crowded_race_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("tenure", "crowded", "YEAR"),
  percent_group_by = ("YEAR")
) |>
  filter(crowded)
