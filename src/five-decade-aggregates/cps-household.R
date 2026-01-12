# cps-household-bedroom-crowding.R
# The purpose of this script is to aggregate average household size, number of 
# bedrooms, and crowding by decade in the CPS

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums_cps.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_decade <- crosstab_mean(
  data = ipums_person, # no GQ variable?
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = c("YEAR")
)
