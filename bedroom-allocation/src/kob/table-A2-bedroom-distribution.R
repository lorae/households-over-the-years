# table-A2-bedroom-distribution.R
#
# Appendix Table A2: Distribution of bedroom counts of occupied housing
# units by tenure and unit age, 2020. Percentages within each row sum to
# ~100% (within rounding). Rounded to whole numbers.
#
# Rows: {Built before 1970, Built since 1970} x {Renter, Owner}
# Cols: 0, 1, 2, 3, 4, 5+ bedrooms
#
# Inputs:  bedroom-allocation/throughput/households.duckdb
# Outputs:
#   bedroom-allocation/output/tables/table-A2-bedroom-distribution.csv
#   bedroom-allocation/output/tables/table-A2-bedroom-distribution.xlsx
#     (if the writexl package is available)

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)
library(tidyr)
library(readr)

devtools::load_all("../demographr")

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb",
  read_only = TRUE
)

pre_1970_cohorts <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969"
)

dist <- tbl(con, "households") |>
  filter(
    decade == 2020L,
    !is.na(bedrooms_recode),
    !is.na(build_cohort),
    !is.na(tenure)
  ) |>
  mutate(
    cohort_group = if_else(
      build_cohort %in% pre_1970_cohorts,
      "Built before 1970",
      "Built since 1970"
    )
  ) |>
  crosstab_percent(
    wt_col           = "HHWT",
    group_by         = c("cohort_group", "tenure", "bedrooms_recode"),
    percent_group_by = c("cohort_group", "tenure")
  ) |>
  collect()

dbDisconnect(con)

table_a2 <- dist |>
  mutate(
    bedroom_col = if_else(bedrooms_recode == 5L, "5+", as.character(bedrooms_recode)),
    pct_rounded = round(percent, 1)
  ) |>
  select(cohort_group, tenure, bedroom_col, pct_rounded) |>
  pivot_wider(
    names_from  = bedroom_col,
    values_from = pct_rounded,
    values_fill = 0
  ) |>
  mutate(
    tenure = case_when(
      tenure == "renter" ~ "Renter",
      tenure == "owner"  ~ "Owner"
    )
  ) |>
  arrange(
    factor(cohort_group, levels = c("Built before 1970", "Built since 1970")),
    factor(tenure,       levels = c("Renter", "Owner"))
  ) |>
  select(cohort_group, tenure, `0`, `1`, `2`, `3`, `4`, `5+`)

print(table_a2)

# ----- Save -----
output_dir <- "bedroom-allocation/output/tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(table_a2, file.path(output_dir, "table-A2-bedroom-distribution.csv"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    table_a2,
    file.path(output_dir, "table-A2-bedroom-distribution.xlsx")
  )
  message("Saved CSV + XLSX to bedroom-allocation/output/tables/")
} else {
  message("Saved CSV. Install writexl to also produce xlsx.")
}
