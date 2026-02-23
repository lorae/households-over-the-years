# generate-relative-crowding-risk.R

library(dplyr)
library(readr)

# ----------------------------
# Income (ref = $150,000 and greater)
# ----------------------------
crowded_income <- read_csv(
  "five-decade-aggregates/output/raw/crowded_income_everyone.csv",
  show_col_types = FALSE
) |>
  group_by(YEAR) |>
  mutate(
    rel_risk = percent_crowded /
      percent_crowded[hhincome_2020_binned == "$150,000 and greater"]
  ) |>
  ungroup()

write_csv(
  crowded_income,
  "five-decade-aggregates/output/raw/crowded_rel_risk_income.csv"
)

# ----------------------------
# Race (ref = White)
# ----------------------------
crowded_race <- read_csv(
  "five-decade-aggregates/output/raw/crowded_race.csv",
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
  "five-decade-aggregates/output/raw/crowded_rel_risk_race.csv"
)

# ----------------------------
# Tenure (ref = owner)
# ----------------------------
crowded_tenure <- read_csv(
  "five-decade-aggregates/output/raw/crowded_tenure.csv",
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
  "five-decade-aggregates/output/raw/crowded_rel_risk_tenure.csv"
)
