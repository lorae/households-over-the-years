# generate-relative-crowding-risk.R

library(dplyr)
library(readr)

# ----------------------------
# Income (ref = $150,000 and greater)
# ----------------------------
crowded_income <- read_csv(
  "output/five-decade-tables/raw/crowded_income_adults.csv",
  show_col_types = FALSE
) |>
  group_by(YEAR) |>
  mutate(
    rel_risk = percent_crowded /
      percent_crowded[inctot_binned == "$150,000 and greater"]
  ) |>
  ungroup()

write_csv(
  crowded_income,
  "output/five-decade-tables/raw/crowded_rel_risk_income_adult.csv"
)

# ----------------------------
# Race (ref = White)
# ----------------------------
crowded_race <- read_csv(
  "output/five-decade-tables/raw/crowded_race.csv",
  show_col_types = FALSE
) |>
  group_by(YEAR) |>
  mutate(
    rel_risk = percent_crowded /
      percent_crowded[race_eth == "White"]
  ) |>
  ungroup()

write_csv(
  crowded_race,
  "output/five-decade-tables/raw/crowded_rel_risk_race.csv"
)

# ----------------------------
# Tenure (ref = owner)
# ----------------------------
crowded_tenure <- read_csv(
  "output/five-decade-tables/raw/crowded_tenure.csv",
  show_col_types = FALSE
) |>
  group_by(YEAR) |>
  mutate(
    rel_risk = percent_crowded /
      percent_crowded[tenure == "owner"]
  ) |>
  ungroup()

write_csv(
  crowded_tenure,
  "output/five-decade-tables/raw/crowded_rel_risk_tenure.csv"
)
