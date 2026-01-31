# table-ppbr-sociodemo-group

library(dplyr)
library(tidyr)
library(readr)
library(writexl)

# ----------------------------
# Read ppbr data
# ----------------------------
ppbr_overall <- read_csv(
  "output/five-decade-tables/raw/ppbr_overall.csv",
  show_col_types = FALSE
)

ppbr_race <- read_csv(
  "output/five-decade-tables/raw/ppbr_race.csv",
  show_col_types = FALSE
)

ppbr_income <- read_csv(
  "output/five-decade-tables/raw/ppbr_income_everyone.csv",
  show_col_types = FALSE
)

ppbr_tenure <- read_csv(
  "output/five-decade-tables/raw/ppbr_tenure.csv",
  show_col_types = FALSE
)

ppbr_birthplace <- read_csv(
  "output/five-decade-tables/raw/ppbr_birthplace.csv",
  show_col_types = FALSE
)

# ----------------------------
# Helper: section with header + data + spacer
# ----------------------------
make_section <- function(df, group_name, category_col) {
  wide <- df |>
    transmute(
      group = group_name,
      category = .data[[category_col]],
      YEAR,
      persons_per_bedroom
    ) |>
    pivot_wider(
      names_from = YEAR,
      values_from = persons_per_bedroom
    )
  
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
overall_section <- ppbr_overall |>
  transmute(
    group = "Overall",
    category = "Overall",
    YEAR,
    persons_per_bedroom
  ) |>
  pivot_wider(
    names_from = YEAR,
    values_from = persons_per_bedroom
  )

# spacer after overall
year_cols <- setdiff(names(overall_section), c("group", "category"))
overall_spacer <- tibble(group = "", category = "")
overall_spacer[year_cols] <- NA_real_

overall_section <- bind_rows(overall_section, overall_spacer)

# ----------------------------
# Other sections
# ----------------------------
race_section <- make_section(ppbr_race, "Race / Ethnicity", "race_eth")
income_section <- make_section(ppbr_income, "Income", "inctot_binned")
tenure_section <- make_section(ppbr_tenure, "Tenure", "tenure")
birthplace_section <- make_section(ppbr_birthplace, "Place of Birth", "birthplace")

# ----------------------------
# Combine + export
# ----------------------------
export_tbl <- bind_rows(
  overall_section,
  race_section,
  income_section,
  tenure_section,
  birthplace_section
)

write_xlsx(
  export_tbl,
  "output/five-decade-tables/ppbr_by_group.xlsx"
)