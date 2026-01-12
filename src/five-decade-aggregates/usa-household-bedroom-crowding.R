# usa-household-bedroom-crowding.R
# The purpose of this script is to aggregate average household size, number of 
# bedrooms, and crowding by decade.

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_decade <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 