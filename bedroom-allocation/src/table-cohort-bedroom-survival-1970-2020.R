# table-cohort-bedroom-survival-1970-2020.R
#
# Cohort survival table: weighted occupied housing units by build cohort ×
# bedroom count × decade, plus a per-(cohort × bedroom) retention rate
# normalized to the first decade in which the cohort is fully observed.
#
# Tests the hypothesis that within a given build cohort, large-bedroom units
# lose occupancy faster than small-bedroom units over subsequent decades —
# i.e. people increasingly choose smaller-bedroom units even when more
# bedrooms are being built.
#
# Inputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/output/tables/table-cohort-bedroom-survival-1970-2020.csv

library(dplyr)
library(dbplyr)
library(duckdb)
devtools::load_all("../demographr")

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb",
  read_only = TRUE
)
households <- tbl(con, "households")

# --- weighted counts by decade × cohort × bedrooms ---

cell_counts <- households |>
  filter(!is.na(bedrooms_recode), !is.na(build_cohort)) |>
  crosstab_count(
    wt_col = "HHWT",
    group_by = c("decade", "build_cohort", "bedrooms_recode")
  ) |>
  collect()

dbDisconnect(con)

cohort_order <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969",
  "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009",
  "2010 - 2019", "2020 onward"
)

# Per-cohort baseline = first decade the cohort is fully observed. Mirrors the
# logic in fig02-housing-stock-by-build-cohort: pre-1970 cohorts use 1970,
# post-1970 cohorts use the decade in which their construction window closes.
# "2020 onward" never closes within this range, so its retention is left NA.
baseline_decade <- tibble(
  build_cohort    = cohort_order,
  baseline_decade = c(1970, 1970, 1970, 1970, 1980, 1990, 2000, 2010, 2020, NA_real_)
)

baseline_counts <- cell_counts |>
  inner_join(baseline_decade, by = "build_cohort") |>
  filter(decade == baseline_decade) |>
  select(build_cohort, bedrooms_recode, baseline_count = weighted_count)

survival <- cell_counts |>
  left_join(baseline_decade,  by = "build_cohort") |>
  left_join(baseline_counts,  by = c("build_cohort", "bedrooms_recode")) |>
  mutate(
    retention_pct = if_else(
      !is.na(baseline_decade) & decade >= baseline_decade,
      weighted_count / baseline_count,
      NA_real_
    ),
    build_cohort = factor(build_cohort, levels = cohort_order)
  ) |>
  arrange(build_cohort, bedrooms_recode, decade) |>
  select(
    build_cohort, bedrooms_recode, decade, baseline_decade,
    weighted_count, baseline_count, retention_pct
  )

write.csv(
  survival,
  "bedroom-allocation/output/tables/table-cohort-bedroom-survival-1970-2020.csv",
  row.names = FALSE
)
