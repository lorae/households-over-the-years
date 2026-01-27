
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

clean_crowding_output <- function(df) {
  df |>
    filter(crowded) |>
    select(-crowded) |>
    mutate(
      YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)
    ) |>
    rename(percent_crowded = percent) |>
    arrange(YEAR)
}

years <- c(1970, 1980, 1990, 2000, 2010, 2020)

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

# ================================
# Raw crosstabs
# ================================

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ====================
# --- Overall ---
# ====================

# crowded
crowded_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "YEAR"),
  percent_group_by = c("YEAR")
)

crowded_overall <- clean_crowding_output(crowded_decade_usa)

readr::write_csv(
  crowded_overall,
  "output/five-decade-tables/raw/crowded_overall.csv"
)

# persons per bedroom
ppbr_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = "YEAR"
)

ppbr_overall <- ppbr_decade_usa |>
  mutate(YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)) |>
  rename(persons_per_bedroom = weighted_mean) |>
  arrange(YEAR)

readr::write_csv(
  ppbr_overall,
  "output/five-decade-tables/raw/ppbr_overall.csv"
)

# ====================
# --- By Race ---
# ====================

# crowded
crowded_race_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("race_eth", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "race_eth")
)

crowded_race <- clean_crowding_output(crowded_race_decade_usa)

readr::write_csv(
  crowded_race,
  "output/five-decade-tables/raw/crowded_race.csv"
)

# persons per bedroom
ppbr_race_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("race_eth", "YEAR")
)

ppbr_race <- ppbr_race_decade_usa |>
  mutate(YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)) |>
  rename(persons_per_bedroom = weighted_mean) |>
  arrange(YEAR)

readr::write_csv(
  ppbr_race,
  "output/five-decade-tables/raw/ppbr_race.csv"
)

# ====================
# --- By Tenure ---
# ====================

# crowded
crowded_tenure_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("tenure", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "tenure")
)

crowded_tenure <- clean_crowding_output(crowded_tenure_decade_usa)

readr::write_csv(
  crowded_tenure,
  "output/five-decade-tables/raw/crowded_tenure.csv"
)

# persons per bedroom
ppbr_tenure_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("tenure", "YEAR")
)

ppbr_tenure <- ppbr_tenure_decade_usa |>
  mutate(YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)) |>
  rename(persons_per_bedroom = weighted_mean) |>
  arrange(YEAR)

readr::write_csv(
  ppbr_tenure,
  "output/five-decade-tables/raw/ppbr_tenure.csv"
)

# ====================
# --- By Birthplace ---
# ====================

# crowded
crowded_birthplace_decade_usa <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("birthplace", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "birthplace")
)

crowded_birthplace <- clean_crowding_output(crowded_birthplace_decade_usa)

readr::write_csv(
  crowded_birthplace,
  "output/five-decade-tables/raw/crowded_birthplace.csv"
)

# persons per bedroom
ppbr_birthplace_decade_usa <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("birthplace", "YEAR")
)

ppbr_birthplace <- ppbr_birthplace_decade_usa |>
  mutate(YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)) |>
  rename(persons_per_bedroom = weighted_mean) |>
  arrange(YEAR)

readr::write_csv(
  ppbr_birthplace,
  "output/five-decade-tables/raw/ppbr_birthplace.csv"
)

# ====================
# --- By Income, ADULTS ONLY ---
# ====================

ipums_person_adults <- ipums_person |>
  filter(AGE >= 18)

base_data_adults <- ipums_person_adults |> filter(GQ %in% c(0, 1, 2))

# crowded
crowded_income_decade_usa <- crosstab_percent(
  data = base_data_adults,
  wt_col = "PERWT",
  group_by = c("inctot_binned", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "inctot_binned")
)

crowded_income_adults <- clean_crowding_output(crowded_income_decade_usa)

readr::write_csv(
  crowded_income_adults,
  "output/five-decade-tables/raw/crowded_income_adults.csv"
)

# persons per bedroom
ppbr_income_decade_usa <- crosstab_mean(
  data = base_data_adults,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("inctot_binned", "YEAR")
)

ppbr_income_adults <- ppbr_income_decade_usa |>
  mutate(YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)) |>
  rename(persons_per_bedroom = weighted_mean) |>
  arrange(YEAR)

readr::write_csv(
  ppbr_income_adults,
  "output/five-decade-tables/raw/ppbr_income_adults.csv"
)

# ------ Old
# ================================
# 1. Overall crowding
# ================================

overall <- clean_years(crowded_decade_usa) |>
  filter(!is.na(crowded))

overall_percent <- overall |>
  mutate(row = case_when(
    crowded ~ "Crowded: Percent > 2 persons per bedroom",
    !crowded ~ "Not crowded: Percent <= 2 persons per bedroom"
  )) |>
  select(row, YEAR, value = percent)

overall_count <- overall |>
  group_by(YEAR) |>
  summarise(value = sum(count), .groups = "drop") |>
  mutate(row = "Number of observations") |>
  select(row, YEAR, value)

overall_table <- bind_rows(overall_percent, overall_count)

# ================================
# 2. Race / ethnicity (crowded only)
# ================================

race_levels <- c(
  "AIAN", "AAPI", "Black", "Hispanic",
  "White", "Multiracial", "Other"
)

race_table <- clean_years(crowded_race_decade_usa) |>
  filter(crowded, race_eth %in% race_levels) |>
  mutate(row = race_eth) |>
  select(row, YEAR, value = percent)

# ================================
# 3. Tenure (crowded only)
# ================================

tenure_table <- clean_years(crowded_tenure_decade_usa) |>
  filter(crowded) |>
  mutate(row = if_else(tenure == "owner", "Owner", "Renter")) |>
  select(row, YEAR, value = percent)

# ================================
# 4. Birthplace (crowded only)
# ================================

birthplace_table <- clean_years(crowded_birthplace_decade_usa) |>
  filter(crowded) |>
  mutate(row = if_else(birthplace == "U.S.-born", "U.S.-Born", "Foreign-Born")) |>
  select(row, YEAR, value = percent)

# ================================
# 5. Income (crowded only, adults)
# ================================

income_levels <- c(
  "less than $50,000",
  "$50,000 - $99,999",
  "$100,000 - $149,999",
  "$150,000 and greater"
)

income_table <- clean_years(crowded_income_decade_usa) |>
  filter(crowded, inctot_binned %in% income_levels) |>
  mutate(
    row = factor(inctot_binned, levels = income_levels)
  ) |>
  arrange(row) |>
  mutate(row = as.character(row)) |>
  select(row, YEAR, value = percent)


# ================================
# 6. Combine (wide, no blanks yet)
# ================================

final_wide <- bind_rows(
  overall_table,
  race_table,
  tenure_table,
  birthplace_table,
  income_table
) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  ) |>
  select(row, `1970`, `1980`, `1990`, `2000`, `2010`, `2020`)

# ================================
# 7. Insert blank separator rows
# ================================

blank <- tibble(
  row = "",
  `1970` = NA_real_,
  `1980` = NA_real_,
  `1990` = NA_real_,
  `2000` = NA_real_,
  `2010` = NA_real_,
  `2020` = NA_real_
)

final_table <- bind_rows(
  final_wide |> slice(1:3),    # overall
  blank,
  final_wide |> slice(4:10),   # race
  blank,
  final_wide |> slice(11:12),  # tenure
  blank,
  final_wide |> slice(13:14),  # birthplace
  blank,
  final_wide |> slice(15:18)   # income (adults only)
)

# ================================
# 8. Export Excel
# ================================

dir.create("output/five-decade-tables", recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  final_table,
  "output/five-decade-tables/usa_crowding_by_subgroup_and_decade_raw.xlsx"
)