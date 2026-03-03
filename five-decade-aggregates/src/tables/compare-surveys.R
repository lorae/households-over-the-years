# compare-surveys.R
#
# Compares ACS and CPS surveys side-by-side: population counts, group quarters
# shares, and household size by decade.
#
# Inputs:
# - data/five-decade-db/ipums.duckdb (table: ipums_person)
# - data/five-decade-db/ipums_cps.duckdb (table: ipums_person)
# - ../demographr (sibling package)
#
# Outputs:
# - five-decade-aggregates/output/raw/compare-surveys.csv
#
#
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

# ================================
# ACS: GQ share + HH size (non-GQ)
# ================================
ipums_person_acs <- tbl(con_acs, "ipums_person") |>
  mutate(in_gq = !GQ %in% c(0, 1, 2))

count_acs <- crosstab_count(
  data = ipums_person_acs,
  wt_col = "PERWT",
  group_by = "YEAR"
) |>
  collect() |>
  left_join(acs_year_map, by = "YEAR") |>
  mutate(YEAR = YEAR_OUT) |>
  select(-YEAR_OUT) |>
  arrange(YEAR)

gq_decade_usa <- crosstab_percent(
  data = ipums_person_acs,
  wt_col = "PERWT",
  group_by = c("YEAR", "in_gq"),
  percent_group_by = c("YEAR")
) |>
  filter(in_gq) |>
  select(YEAR, percent_in_gq = percent) |>
  left_join(acs_year_map, by = "YEAR") |>
  mutate(YEAR = YEAR_OUT) |>
  select(-YEAR_OUT) |>
  arrange(YEAR)

hhsize_decade_usa <- crosstab_mean(
  data = ipums_person_acs |> filter(!in_gq),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("YEAR")
) |>
  left_join(acs_year_map, by = "YEAR") |>
  mutate(YEAR = YEAR_OUT) |>
  select(-YEAR_OUT) |>
  arrange(YEAR)

# ================================
# CPS: connect DB
# ================================
con_cps <- dbConnect(
  duckdb::duckdb(),
  "data/five-decade-db/ipums_cps.duckdb"
)

# ================================
# CPS: GQ share + HH size (non-GQ)
# ================================
ipums_person_cps <- tbl(con_cps, "ipums_person") |>
  mutate(in_gq = !GQ %in% c(0, 1))

count_cps <- crosstab_count(
  data = ipums_person_cps,
  wt_col = "ASECWT",
  group_by = "YEAR"
) |>
  arrange(YEAR)

gq_decade_cps <- crosstab_percent(
  data = ipums_person_cps,
  wt_col = "ASECWT",
  group_by = c("YEAR", "in_gq"),
  percent_group_by = c("YEAR")
) |>
  filter(in_gq) |>
  select(YEAR, percent_in_gq = percent, count, weighted_count) |>
  arrange(YEAR)

hhsize_decade_cps <- crosstab_mean(
  data = ipums_person_cps |> filter(!in_gq),
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |>
  arrange(YEAR)

# ================================
# COLLECT ALL TABLES (ACS + CPS)
# ================================
count_acs        <- count_acs        |> collect()
gq_decade_usa    <- gq_decade_usa    |> collect()
hhsize_decade_usa<- hhsize_decade_usa|> collect()

count_cps        <- count_cps        |> collect()
gq_decade_cps    <- gq_decade_cps    |> collect()
hhsize_decade_cps<- hhsize_decade_cps|> collect()

# ================================
# Combine
# ================================


final_by_year <- count_acs |>
  rename(
    count_usa = count,
    weighted_count_usa = weighted_count
  ) |>
  left_join(
    count_cps |>
      rename(
        count_cps = count,
        weighted_count_cps = weighted_count
      ),
    by = "YEAR"
  ) |>
  left_join(
    hhsize_decade_usa |>
      select(YEAR, hhsize_usa = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    hhsize_decade_cps |>
      select(YEAR, hhsize_cps = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    gq_decade_usa |>
      rename(gq_usa = percent_in_gq),
    by = "YEAR"
  ) |>
  left_join(
    gq_decade_cps |>
      rename(gq_cps = percent_in_gq),
    by = "YEAR"
  ) |>
  arrange(YEAR)

final_by_year


# ================================
# Save
# ================================
write_csv(
  final_by_year,
  "five-decade-aggregates/output/raw/compare-surveys.csv"
)
# ================================
# Disconnect
# ================================
dbDisconnect(con_acs)

dbDisconnect(con_cps)
