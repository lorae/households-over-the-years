# fig05-table1-sample-composition-by-decade.R
#
# Table 1: weighted descriptive statistics of single-mother households
# by decade. All stats use HHWT except the unweighted sample count.
#
# Inputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)
#
# Outputs:
# - household-archetypes/output/tables/fig05-table1-sample-composition-by-decade.csv
#
# TODO: inflation-adjust HHINCOME using five-decade-aggregates/reference/inflators-1970-2020.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(tidyr)
devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb", read_only = TRUE)

single_mothers <- tbl(con, "single_mothers")

# --- householder-level data with derived columns ---

householders <- single_mothers |>
  filter(RELATE == 1) |>
  mutate(
    decade = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    bedrooms_recode = pmin(BEDROOMS - 1L, 5L),
    n_children = NUMPREC - 1L,
    # R-CPI-U-RS values (https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm)
    # 1970 is not covered by the R-CPI-U-RS series, so real income for 1970 is NA.
    cpi_u_rs = case_when(
      decade == 1980L ~ 127.1,
      decade == 1990L ~ 197.6,
      decade == 2000L ~ 252.5,
      decade == 2010L ~ 336.9, # 2012 value
      decade == 2020L ~ 431.5, # 2022 value
      .default = NA_real_
    ),
    HHINCOME_real_2022 = HHINCOME * (431.5 / cpi_u_rs),
    race_bucket = case_when(
      RACE == 1L ~ "White",
      RACE == 2L ~ "Black",
      RACE == 3L ~ "AIAN",
      RACE %in% c(4L, 5L, 6L) ~ "AAPI",
      RACE == 7L ~ "Other",
      RACE %in% c(8L, 9L) ~ "Multiracial"
    ),
    hispan_binary = as.integer(HISPAN %in% c(1L, 2L, 3L, 4L)),
    tenure = case_when(
      OWNERSHP == 1L ~ "owner",
      OWNERSHP == 2L ~ "renter",
      .default = "other/NA"
    )
  )

# --- unweighted n ---

n_unweighted <- householders |>
  count(decade, name = "value") |>
  collect() |>
  mutate(stat = "n (unweighted)", value = as.numeric(value))

# --- weighted means ---

mean_age <- crosstab_mean(householders, value = "AGE", wt_col = "HHWT", group_by = "decade") |>
  transmute(decade, value = weighted_mean, stat = "Mean householder age")

mean_income <- crosstab_mean(householders, value = "HHINCOME", wt_col = "HHWT", group_by = "decade") |>
  transmute(decade, value = weighted_mean, stat = "Mean HHINCOME (nominal $)")

mean_income_real <- crosstab_mean(
  householders |> filter(!is.na(HHINCOME_real_2022)),
  value = "HHINCOME_real_2022",
  wt_col = "HHWT",
  group_by = "decade"
) |>
  transmute(decade, value = weighted_mean, stat = "Mean HHINCOME (2022 $, R-CPI-U-RS)")

mean_bedrooms <- crosstab_mean(householders, value = "bedrooms_recode", wt_col = "HHWT", group_by = "decade") |>
  transmute(decade, value = weighted_mean, stat = "Mean bedrooms (0 = studio, 5 = 5+)")

mean_children <- crosstab_mean(householders, value = "n_children", wt_col = "HHWT", group_by = "decade") |>
  transmute(decade, value = weighted_mean, stat = "Mean # children")

# --- weighted shares ---

tenure_shares <- crosstab_percent(
  householders,
  wt_col = "HHWT",
  group_by = c("decade", "tenure"),
  percent_group_by = "decade"
) |>
  transmute(decade, value = percent, stat = paste0("% ", tenure))

race_bucket_shares <- crosstab_percent(
  householders,
  wt_col = "HHWT",
  group_by = c("decade", "race_bucket"),
  percent_group_by = "decade"
) |>
  transmute(decade, value = percent, stat = paste0("% ", race_bucket))

hispan_share <- crosstab_mean(
  householders,
  value = "hispan_binary",
  wt_col = "HHWT",
  group_by = "decade"
) |>
  transmute(decade, value = weighted_mean * 100, stat = "% Hispanic (any race_bucket)")

# --- assemble into wide table, rows = stats, cols = decades ---

table1 <- bind_rows(
  n_unweighted,
  mean_age,
  mean_income,
  mean_income_real,
  mean_bedrooms,
  mean_children,
  tenure_shares,
  race_bucket_shares,
  hispan_share
) |>
  pivot_wider(names_from = decade, values_from = value) |>
  select(stat, any_of(as.character(c(1970, 1980, 1990, 2000, 2010, 2020))))

print(table1, n = Inf)

write.csv(
  table1,
  "household-archetypes/output/tables/fig05-table1-sample-composition-by-decade.csv",
  row.names = FALSE
)

dbDisconnect(con)
