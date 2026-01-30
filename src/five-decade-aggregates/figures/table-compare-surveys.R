# table-compare-surveys.R

# table-compare-surveys.R

library(dplyr)
library(readr)
library(tidyr)
library(writexl)

in_path  <- "output/five-decade-tables/raw/compare-surveys.csv"
out_path <- "output/five-decade-tables/compare-surveys.xlsx"

compare_surveys <- read_csv(in_path, show_col_types = FALSE)

final_table <- compare_surveys |>
  mutate(
    YEAR = as.character(YEAR)
  ) |>
  select(
    YEAR,
    count_usa,
    weighted_count_usa,
    count_cps,
    weighted_count_cps,
    hhsize_usa,
    hhsize_cps,
    gq_usa,
    gq_cps
  ) |>
  pivot_longer(
    cols = -YEAR,
    names_to = "row",
    values_to = "value"
  ) |>
  mutate(
    row = recode(
      row,
      count_usa = "ACS sample (unweighted)",
      weighted_count_usa = "ACS population (weighted)",
      count_cps = "CPS sample (unweighted)",
      weighted_count_cps = "CPS population (weighted)",
      hhsize_usa = "ACS average household size (non-GQ)",
      hhsize_cps = "CPS average household size (non-GQ)",
      gq_usa = "ACS percent in group quarters",
      gq_cps = "CPS percent in group quarters"
    )
  ) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  )

dir.create("output/five-decade-tables", recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list("compare_surveys" = final_table),
  path = out_path
)

