# generate-household-bedroom-crowding-overall.R
# Aggregate household size, bedrooms, and crowding by decade (ACS + CPS)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "data/five-decade-db"
out_dir  <- "output/five-decade-tables/raw"

# ----------------------------
# Libraries
# ----------------------------
library(dplyr)
library(duckdb)
library(dbplyr)
library(readr)
library(tidyr)

devtools::load_all("../demographr")

# ----------------------------
# Target decades
# ----------------------------
target_years <- c(1970, 1980, 1990, 2000, 2010, 2020)

acs_year_map <- tibble(
  YEAR = c(1970, 1980, 1990, 2000, 2012, 2022),
  decade = c(1970, 1980, 1990, 2000, 2010, 2020)
)

# ============================================================
# ACS
# ============================================================

con_acs <- dbConnect(
  duckdb::duckdb(),
  file.path(data_dir, "ipums.duckdb")
)

acs_person <- tbl(con_acs, "ipums_person") |>
  filter(GQ %in% c(0, 1, 2))

acs_hhsize <- crosstab_mean(
  data     = acs_person,
  value    = "NUMPREC",
  wt_col   = "PERWT",
  group_by = "YEAR"
) |>
  rename(hhsize = weighted_mean)

acs_bedrooms <- crosstab_mean(
  data     = acs_person,
  value    = "bedroom",
  wt_col   = "PERWT",
  group_by = "YEAR"
) |>
  rename(bedroom = weighted_mean)

acs_ppbr <- crosstab_mean(
  data     = acs_person,
  value    = "ppbr",
  wt_col   = "PERWT",
  group_by = "YEAR"
) |>
  rename(ppbr = weighted_mean)

# ============================================================
# CPS
# ============================================================

con_cps <- dbConnect(
  duckdb::duckdb(),
  file.path(data_dir, "ipums_cps.duckdb")
)

cps_person <- tbl(con_cps, "ipums_person")

cps_hhsize <- crosstab_mean(
  data     = cps_person,
  value    = "NUMPREC",
  wt_col   = "ASECWT",
  group_by = "YEAR"
)

# ----------------------------
# Save outputs
# ----------------------------

hhsize_decade_overall <- acs_hhsize
bedroom_decade_overall <- acs_bedrooms
ppbr_decade_overall <- acs_ppbr

cps_hhsize_decade_overall <- cps_hhsize


write_csv(hhsize_decade_overall, file.path(out_dir, "hhsize_decade_overall.csv"))
write_csv(bedroom_decade_overall, file.path(out_dir, "bedroom_decade_overall.csv"))
write_csv(ppbr_decade_overall, file.path(out_dir, "ppbr_decade_overall.csv"))

write_csv(cps_hhsize_decade_overall, file.path(out_dir, "cps_hhsize_decade_overall.csv"))

