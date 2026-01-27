# build-hhsize-change-table.R
library(dplyr)
library(readr)
library(tidyr)

# ----- Step 1: Read data ----- #
hh <- read_csv("output/five-decade-tables/raw/hhsize_state_decade.csv",
               show_col_types = FALSE)

# Expected columns (from your pipeline):
# STATEFIP, state_name, YEAR, count, weighted_count, weighted_mean

# ----- Step 2: Keep only 1970 and 2020 ----- #
hh_70_20 <- hh |>
  filter(YEAR %in% c(1970, 2020)) |>
  select(
    STATEFIP,
    state_name,
    YEAR,
    count,
    weighted_count,
    hhsize = weighted_mean
  )

# ----- Step 3: Pivot wide ----- #
hh_wide <- hh_70_20 |>
  pivot_wider(
    names_from = YEAR,
    values_from = c(count, weighted_count, hhsize),
    names_glue = "{.value}_{YEAR}"
  )

# ----- Step 4: Compute changes ----- #
hh_change <- hh_wide |>
  mutate(
    change_hhsize = hhsize_2020 - hhsize_1970,
    pct_change_hhsize = 100 * (hhsize_2020 - hhsize_1970) / hhsize_1970
  ) |>
  arrange(state_name)

# ----- Step 5: Result ----- #
hh_change