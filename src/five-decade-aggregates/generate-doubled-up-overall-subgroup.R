# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("writexl")

devtools::load_all("../demographr")

# ================================
# Helpers
# ================================

clean_years <- function(df) {
  df |>
    mutate(
      YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)
    )
}

clean_binary_percent_output <- function(
    df,
    outcome_var,
    outcome_name,
    group_var = NULL,
    group_levels = NULL
) {
  out <- df |>
    filter(.data[[outcome_var]]) |>
    select(-all_of(outcome_var)) |>
    clean_years() |>
    rename(!!paste0("percent_", outcome_name) := percent)
  
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

years <- c(1970, 1980, 1990, 2000, 2010, 2020)

race_levels <- c(
  "AIAN", "AAPI", "Black", "Hispanic",
  "White", "Multiracial", "Other"
)

tenure_levels <- c("owner", "renter")

birthplace_levels <- c("U.S.-born", "foreign-born")

income_levels <- c(
  "less than $50,000",
  "$50,000 - $99,999",
  "$100,000 - $149,999",
  "$150,000 and greater"
)

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person")

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ================================
# Raw crosstabs
# ================================

# ====================
# Overall
# ====================

doubled_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("is_multifam", "YEAR"),
  percent_group_by = c("YEAR")
)

doubled_overall <- clean_binary_percent_output(
  doubled_decade_usa,
  outcome_var  = "is_multifam",
  outcome_name = "doubled"
)

write_csv(
  doubled_overall,
  "output/five-decade-tables/raw/doubled_overall.csv"
)

nonprime_decade_us <- crosstab_mean(
  data = base_data,
  value = 
  wt_col = "PERWT",
  group_by = c("")
)

# ====================
# By Race
# ====================

doubled_race_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("race_eth", "is_multifam", "YEAR"),
  percent_group_by = c("YEAR", "race_eth")
)

doubled_race <- clean_binary_percent_output(
  doubled_race_decade_usa,
  outcome_var  = "is_multifam",
  outcome_name = "doubled",
  group_var    = "race_eth",
  group_levels = race_levels
)

write_csv(
  doubled_race,
  "output/five-decade-tables/raw/doubled_race.csv"
)

# ====================
# By Tenure
# ====================

doubled_tenure_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("tenure", "is_multifam", "YEAR"),
  percent_group_by = c("YEAR", "tenure")
)

doubled_tenure <- clean_binary_percent_output(
  doubled_tenure_decade_usa,
  outcome_var  = "is_multifam",
  outcome_name = "doubled",
  group_var    = "tenure",
  group_levels = tenure_levels
)

write_csv(
  doubled_tenure,
  "output/five-decade-tables/raw/doubled_tenure.csv"
)

# ====================
# By Birthplace
# ====================

doubled_birthplace_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("birthplace", "is_multifam", "YEAR"),
  percent_group_by = c("YEAR", "birthplace")
)

doubled_birthplace <- clean_binary_percent_output(
  doubled_birthplace_decade_usa,
  outcome_var  = "is_multifam",
  outcome_name = "doubled",
  group_var    = "birthplace",
  group_levels = birthplace_levels
)

write_csv(
  doubled_birthplace,
  "output/five-decade-tables/raw/doubled_birthplace.csv"
)

# ====================
# By Income (Adults Only)
# ====================

ipums_person_adults <- ipums_person |> filter(AGE >= 18)
base_data_adults <- ipums_person_adults |> filter(GQ %in% c(0, 1, 2))

doubled_income_decade_usa <- crosstab_percent(
  data = base_data_adults,
  wt_col = "PERWT",
  group_by = c("inctot_binned", "is_multifam", "YEAR"),
  percent_group_by = c("YEAR", "inctot_binned")
)

doubled_income_adults <- clean_binary_percent_output(
  doubled_income_decade_usa,
  outcome_var  = "is_multifam",
  outcome_name = "doubled",
  group_var    = "inctot_binned",
  group_levels = income_levels
)

write_csv(
  doubled_income_adults,
  "output/five-decade-tables/raw/doubled_income_adults.csv"
)