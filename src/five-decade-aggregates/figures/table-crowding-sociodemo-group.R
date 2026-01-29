# table-crowding-sociodemo-group

library(dplyr)
library(tidyr)
library(writexl)


crowding_overall <- read_csv("output/five-decade-tables/raw/crowded_overall.csv")

crowding_race <- read_csv("output/five-decade-tables/raw/crowded_race.csv")

crowding_income <- read_csv("output/five-decade-tables/raw/crowded_income_adults.csv")

crowding_tenure <- read_csv("output/five-decade-tables/raw/crowded_tenure.csv")

crowding_birthplace <- read_csv("output/five-decade-tables/raw/crowded_birthplace.csv")
# ----------------------------
# Helper: section with header + data + spacer
# ----------------------------
make_section <- function(df, group_name, category_col) {
  wide <- df |>
    transmute(
      group = group_name,
      category = .data[[category_col]],
      YEAR,
      percent_crowded
    ) |>
    pivot_wider(names_from = YEAR, values_from = percent_crowded)
  
  
  year_cols <- setdiff(names(wide), c("group", "category"))
  
  
  header_row <- tibble(group = group_name, category = "")
  header_row[year_cols] <- NA_real_
  
  
  spacer_row <- tibble(group = "", category = "")
  spacer_row[year_cols] <- NA_real_
  
  
  bind_rows(header_row, wide, spacer_row)
}


# ----------------------------
# Overall (NO header row; just data + spacer)
# ----------------------------
overall_section <- crowding_overall |>
  transmute(
    group = "Overall",
    category = "Overall",
    YEAR,
    percent_crowded
  ) |>
  pivot_wider(names_from = YEAR, values_from = percent_crowded)


# spacer after overall
year_cols <- setdiff(names(overall_section), c("group", "category"))
overall_spacer <- tibble(group = "", category = "")
overall_spacer[year_cols] <- NA_real_


overall_section <- bind_rows(overall_section, overall_spacer)

# ----------------------------
# Other sections
# ----------------------------
race_section <- make_section(crowding_race, "Race / Ethnicity", "race_eth")
income_section <- make_section(crowding_income, "Income", "inctot_binned")
tenure_section <- make_section(crowding_tenure, "Tenure", "tenure")
birthplace_section <- make_section(crowding_birthplace, "Place of Birth", "birthplace")


export_tbl <- bind_rows(
  overall_section,
  race_section,
  income_section,
  tenure_section,
  birthplace_section
)

# ----------------------------
# Export
# ----------------------------
write_xlsx(export_tbl, "output/five-decade-tables/crowding_percent_by_group.xlsx")