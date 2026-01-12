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
  percent_group_by = ("YEAR")
) 

# ----- Crowding table export ----- #
library(dplyr)
library(tidyr)
library(writexl)

# ---- Clean + recode ----
crowded_clean <- crowded_decade_usa |>
  filter(!is.na(crowded)) |>
  mutate(
    YEAR = ifelse(YEAR == 2022, 2020, YEAR),
    row = case_when(
      crowded == FALSE ~ "<= 2 persons per bedroom",
      crowded == TRUE  ~ "> 2 persons per bedroom"
    )
  )

# ---- Percent table ----
percent_table <- crowded_clean |>
  select(row, YEAR, value = percent)

# ---- Count row ----
count_table <- crowded_clean |>
  group_by(YEAR) |>
  summarise(value = sum(count), .groups = "drop") |>
  mutate(row = "Count") |>
  select(row, YEAR, value)

# ---- Combine + reshape ----
final_table <- bind_rows(
  percent_table,
  count_table
) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  ) |>
  select(
    row,
    `1970`, `1980`, `1990`, `2000`, `2020`
  ) |>
  arrange(factor(
    row,
    levels = c(
      "<= 2 persons per bedroom",
      "> 2 persons per bedroom",
      "Count"
    )
  ))

# ---- Write Excel ----
dir.create("output/five-decade-tables", recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  final_table,
  "output/five-decade-tables/usa_crowding_share_by_decade_raw.xlsx"
)
