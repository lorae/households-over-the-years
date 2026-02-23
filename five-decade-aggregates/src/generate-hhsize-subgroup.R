# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("writexl")

devtools::load_all("../demographr")
source("five-decade-aggregates/src/helpers/setup.R")

# ================================
# Helpers
# ================================

clean_hhsize_output <- function(df, group_var = NULL, group_levels = NULL) {
  out <- df |>
    clean_years() |>
    rename(hhsize = weighted_mean)
  
  if (!is.null(group_var)) {
    out <- out |>
      mutate(
        !!group_var := factor(.data[[group_var]], levels = group_levels)
      ) |>
      arrange(YEAR, !!sym(group_var))
  } else {
    out <- out |> arrange(YEAR)
  }
  
  out
}

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ================================
# Raw crosstabs
# ================================

# ====================
# Overall
# ====================


hhsize_decade_usa <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = "YEAR"
)

hhsize_overall <- clean_hhsize_output(hhsize_decade_usa)

write_csv(
  hhsize_overall,
  "five-decade-aggregates/output/raw/hhsize_overall.csv"
)

# ====================
# By Race
# ====================

hhsize_race_decade_usa <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("race_eth", "YEAR")
)

hhsize_race <- clean_hhsize_output(
  hhsize_race_decade_usa,
  group_var = "race_eth",
  group_levels = race_levels
)

write_csv(
  hhsize_race,
  "five-decade-aggregates/output/raw/hhsize_race.csv"
)

# ====================
# By Tenure
# ====================

hhsize_tenure_decade_usa <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("tenure", "YEAR")
)

hhsize_tenure <- clean_hhsize_output(
  hhsize_tenure_decade_usa,
  group_var = "tenure",
  group_levels = tenure_levels
)

write_csv(
  hhsize_tenure,
  "five-decade-aggregates/output/raw/hhsize_tenure.csv"
)

# ====================
# By Birthplace
# ====================

hhsize_birthplace_decade_usa <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("birthplace", "YEAR")
)

hhsize_birthplace <- clean_hhsize_output(
  hhsize_birthplace_decade_usa,
  group_var = "birthplace",
  group_levels = birthplace_levels
)

write_csv(
  hhsize_birthplace,
  "five-decade-aggregates/output/raw/hhsize_birthplace.csv"
)

# ====================
# By Income (Adults Only)
# ====================

ipums_person_adults <- ipums_person |> filter(AGE >= 18)
base_data_adults <- ipums_person_adults |> filter(GQ %in% c(0, 1, 2))

hhsize_income_decade_usa <- crosstab_mean(
  data = base_data_adults,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("inctot_binned", "YEAR")
)

hhsize_income_adults <- clean_hhsize_output(
  hhsize_income_decade_usa,
  group_var = "inctot_binned",
  group_levels = income_levels
)

write_csv(
  hhsize_income_adults,
  "five-decade-aggregates/output/raw/hhsize_income_adults.csv"
)