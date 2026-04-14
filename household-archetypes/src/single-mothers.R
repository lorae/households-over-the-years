# single-mothers.R
#
# Identify single-mother households in IPUMS USA data and save to DuckDB
# for downstream scripts.
#
# Inputs:
# - data/five-decade-db/ipums.duckdb (table: ipums)
#
# Outputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)

library(dplyr)
library(dbplyr)
library(duckdb)

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb", read_only = TRUE)

ipums <- tbl(con, "ipums") |>
  filter(GQ %in% c(0,1,2)) |>
  mutate(
    hhid = paste(as.integer(YEAR), as.integer(SERIAL), sep = "-"),
    perid = paste(hhid, as.integer(PERNUM), sep = "-")
  )

# --- get just the single mother + kids households ---

single_mothers <- ipums |>
  filter(NUMPREC > 1) |>
  group_by(hhid) |>
  filter(
    sum(ifelse(RELATE %in% c(1, 3), 0L, 1L)) == 0L, # only householder + children
    max(ifelse(RELATE == 1, SEX, 0L)) == 2L,         # householder is female
    max(ifelse(RELATE == 3, AGE, 0L)) < 18L          # all children under 18
  ) |>
  ungroup() |>
  collect()

# --- save to DuckDB ---

sm_con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb")
dbWriteTable(sm_con, "single_mothers", single_mothers, overwrite = TRUE)
dbDisconnect(sm_con, shutdown = TRUE)

dbDisconnect(con)
