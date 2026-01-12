# usa-household-bedroom-crowding.R
# The purpose of this script is to aggregate average household size, number of 
# bedrooms, and crowding by decade.

# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

crowded_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("crowded", "YEAR"),
  percent_group_by = c("YEAR")
) 

crowded_race_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("race_eth", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "race_eth")
)

crowded_tenure_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("tenure", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "tenure")
) 

crowded_birthplace_decade_usa <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("birthplace", "crowded", "YEAR"),
  percent_group_by = c("YEAR", "birthplace")
) 

# Consolidate findings
crowded_decade_usa
crowded_birthplace_decade_usa
crowded_tenure_decade_usa
crowded_race_decade_usa

library(dplyr)
library(tidyr)
library(writexl)

# ---- Common year cleanup ----
clean_years <- function(df) {
  df |>
    mutate(YEAR = ifelse(YEAR == 2012, 2010, YEAR)) |>
    mutate(YEAR = ifelse(YEAR == 2022, 2020, YEAR))
}

years <- c(1970, 1980, 1990, 2000, 2010, 2020)

# ================================
# 1. Overall crowding table
# ================================

overall <- clean_years(crowded_decade_usa) |>
  filter(!is.na(crowded))

overall_percent <- overall |>
  mutate(row = case_when(
    crowded == TRUE  ~ "Crowded: Percent > 2 persons per bedroom",
    crowded == FALSE ~ "Not crowded: Percent <= 2 persons per bedroom"
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
  "AIAN",
  "AAPI",
  "Black",
  "Hispanic",
  "White",
  "Multiracial",
  "Other"
)

race_table <- clean_years(crowded_race_decade_usa) |>
  filter(crowded == TRUE, race_eth %in% race_levels) |>
  mutate(row = race_eth) |>
  select(row, YEAR, value = percent)

# ================================
# 3. Tenure (crowded only)
# ================================

tenure_table <- clean_years(crowded_tenure_decade_usa) |>
  filter(crowded == TRUE) |>
  mutate(row = case_when(
    tenure == "owner"  ~ "Owner",
    tenure == "renter" ~ "Renter"
  )) |>
  select(row, YEAR, value = percent)

# ================================
# 4. Birthplace (crowded only)
# ================================

birthplace_table <- clean_years(crowded_birthplace_decade_usa) |>
  filter(crowded == TRUE) |>
  mutate(row = case_when(
    birthplace == "U.S.-born"     ~ "U.S.-Born",
    birthplace == "foreign-born" ~ "Foreign-Born"
  )) |>
  select(row, YEAR, value = percent)

# ================================
# 5. Combine (NO blank rows yet)
# ================================

final_wide <- bind_rows(
  overall_table,
  race_table,
  tenure_table,
  birthplace_table
) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  ) |>
  select(
    row,
    `1970`, `1980`, `1990`, `2000`, `2010`, `2020`
  )

# ================================
# 6. Insert blank separator rows (WIDE)
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
  final_wide |> slice(1:3),   # overall rows
  blank,
  final_wide |> slice(4:10),  # race rows
  blank,
  final_wide |> slice(11:12), # tenure rows
  blank,
  final_wide |> slice(13:14)  # birthplace rows
)

# ================================
# 7. Export Excel
# ================================

dir.create("output/five-decade-tables", recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  final_table,
  "output/five-decade-tables/usa_crowding_by_subgroup_and_decade_raw.xlsx"
)
