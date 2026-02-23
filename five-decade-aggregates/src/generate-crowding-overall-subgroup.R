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

clean_crowding_output <- function(df, group_var = NULL, group_levels = NULL) {
  out <- df |>
    filter(crowded) |>
    select(-crowded) |>
    clean_years() |>
    rename(percent_crowded = percent)
  
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

clean_ppbr_output <- function(df, group_var = NULL, group_levels = NULL) {
  out <- df |>
    clean_years() |>
    rename(persons_per_bedroom = weighted_mean)
  
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

crowded_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "YEAR"),
  percent_group_by = c("YEAR")
)

crowded_overall <- clean_crowding_output(crowded_decade_usa)

write_csv(
  crowded_overall,
  "five-decade-aggregates/output/raw/crowded_overall.csv"
)

ppbr_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = "YEAR"
)

ppbr_overall <- clean_ppbr_output(ppbr_decade_usa)

write_csv(
  ppbr_overall,
  "five-decade-aggregates/output/raw/ppbr_overall.csv"
)

# ====================
# By Race
# ====================

crowded_race_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("race_eth", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "race_eth")
)

crowded_race <- clean_crowding_output(
  crowded_race_decade_usa,
  group_var = "race_eth",
  group_levels = race_levels
)

write_csv(
  crowded_race,
  "five-decade-aggregates/output/raw/crowded_race.csv"
)

ppbr_race_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("race_eth", "YEAR")
)

ppbr_race <- clean_ppbr_output(
  ppbr_race_decade_usa,
  group_var = "race_eth",
  group_levels = race_levels
)

write_csv(
  ppbr_race,
  "five-decade-aggregates/output/raw/ppbr_race.csv"
)

# ====================
# By Tenure
# ====================

crowded_tenure_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("tenure", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "tenure")
)

crowded_tenure <- clean_crowding_output(
  crowded_tenure_decade_usa,
  group_var = "tenure",
  group_levels = tenure_levels
)

write_csv(
  crowded_tenure,
  "five-decade-aggregates/output/raw/crowded_tenure.csv"
)

ppbr_tenure_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("tenure", "YEAR")
)

ppbr_tenure <- clean_ppbr_output(
  ppbr_tenure_decade_usa,
  group_var = "tenure",
  group_levels = tenure_levels
)

write_csv(
  ppbr_tenure,
  "five-decade-aggregates/output/raw/ppbr_tenure.csv"
)

# ====================
# By Birthplace
# ====================

crowded_birthplace_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("birthplace", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "birthplace")
)

crowded_birthplace <- clean_crowding_output(
  crowded_birthplace_decade_usa,
  group_var = "birthplace",
  group_levels = birthplace_levels
)

write_csv(
  crowded_birthplace,
  "five-decade-aggregates/output/raw/crowded_birthplace.csv"
)

ppbr_birthplace_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("birthplace", "YEAR")
)

ppbr_birthplace <- clean_ppbr_output(
  ppbr_birthplace_decade_usa,
  group_var = "birthplace",
  group_levels = birthplace_levels
)

write_csv(
  ppbr_birthplace,
  "five-decade-aggregates/output/raw/ppbr_birthplace.csv"
)

# ====================
# By Income (Adults Only)
# ====================

ipums_person_adults <- ipums_person |> filter(AGE >= 18)
base_data_adults <- ipums_person_adults |> filter(GQ %in% c(0, 1, 2))

crowded_income_decade_usa <- crosstab_percent(
  data = base_data_adults,
  wt_col = "PERWT",
  group_by = c("inctot_binned", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "inctot_binned")
)

crowded_income_adults <- clean_crowding_output(
  crowded_income_decade_usa,
  group_var = "inctot_binned",
  group_levels = income_levels
)

write_csv(
  crowded_income_adults,
  "five-decade-aggregates/output/raw/crowded_income_adults.csv"
)

crowded_income_decade_everyone <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2)),
  wt_col = "PERWT",
  group_by = c("hhincome_2020_binned", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "hhincome_2020_binned")
)

crowded_income_everyone <- clean_crowding_output(
  crowded_income_decade_everyone,
  group_var = "hhincome_2020_binned",
  group_levels = income_levels
)

write_csv(
  crowded_income_everyone,
  "five-decade-aggregates/output/raw/crowded_income_everyone.csv"
)

ppbr_income_decade_everyone <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0, 1, 2)),
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("hhincome_2020_binned", "YEAR")
)

ppbr_income_everyone <- ppbr_income_decade_everyone |>
  filter(!is.na(hhincome_2020_binned)) |>  # Remove 12 obs with NA income
  clean_ppbr_output(
    group_var = "hhincome_2020_binned",
    group_levels = income_levels
  )

write_csv(
  ppbr_income_everyone,
  "five-decade-aggregates/output/raw/ppbr_income_everyone.csv"
)