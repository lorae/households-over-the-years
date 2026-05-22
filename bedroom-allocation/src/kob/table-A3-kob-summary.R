# table-A3-kob-summary.R
#
# Appendix Table A3 (first sub-table): KOB top-line summary.
# Columns: 1970 mean, 2020 mean, difference, endowment (e), coefficient (c),
# intercept (u). One row.
#
# Inputs:
#   bedroom-allocation/throughput/kob_output.rds
#   bedroom-allocation/throughput/kob-households.duckdb
# Outputs:
#   bedroom-allocation/output/tables/table-A3-kob-summary.csv
#   bedroom-allocation/output/tables/table-A3-kob-summary.xlsx

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)
library(tibble)
library(readr)

# ----- Step 1: KOB totals from kob_output -----
kob_output <- readRDS("bedroom-allocation/throughput/kob_output.rds")

totals <- kob_output |>
  summarise(
    e_total = sum(e, na.rm = TRUE),
    c_total = sum(c, na.rm = TRUE),
    u_total = sum(u, na.rm = TRUE)
  )

# ----- Step 2: Observed means from microdata (regression sample) -----
con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)

observed <- tbl(con, "households") |>
  filter(
    !is.na(bedrooms_recode),
    !is.na(hoh_age), !is.na(hoh_race_eth), !is.na(hoh_educ_bucket),
    !is.na(hoh_us_born), !is.na(hoh_sex), !is.na(tenure),
    !is.na(region4), !is.na(n_children_under_18), !is.na(n_adults),
    !is.na(hhincome_2020_harmonized)
  ) |>
  group_by(decade) |>
  summarise(
    mean_bedrooms = sum(bedrooms_recode * HHWT) / sum(HHWT),
    .groups = "drop"
  ) |>
  collect()

dbDisconnect(con)

mean_1970 <- observed |> filter(decade == 1970) |> pull(mean_bedrooms)
mean_2020 <- observed |> filter(decade == 2020) |> pull(mean_bedrooms)
gap       <- mean_2020 - mean_1970

# ----- Step 3: Build table -----
table_a3 <- tibble(
  `1970 bedrooms`   = round(mean_1970,       3),
  `2020 bedrooms`   = round(mean_2020,       3),
  `Difference`      = round(gap,             3),
  `Endowment (e)`   = round(totals$e_total,  3),
  `Coefficient (c)` = round(totals$c_total,  3),
  `Intercept (u)`   = round(totals$u_total,  3)
)

print(table_a3)

# ----- Step 4: Save -----
output_dir <- "bedroom-allocation/output/tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(table_a3, file.path(output_dir, "table-A3-kob-summary.csv"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    table_a3,
    file.path(output_dir, "table-A3-kob-summary.xlsx")
  )
  message("Saved CSV + XLSX to bedroom-allocation/output/tables/")
} else {
  message("Saved CSV. Install writexl to also produce xlsx.")
}
