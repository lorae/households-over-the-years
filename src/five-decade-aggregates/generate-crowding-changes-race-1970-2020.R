library(dplyr)
library(readr)
library(tidyr)

# Read data (use repo-relative path, not the URL)
ppbr_race <- readr::read_csv(
  "output/five-decade-tables/raw/ppbr_race.csv",
  show_col_types = FALSE
)

# Keep only 1970 and 2020
ppbr_1970_2020 <- ppbr_race |>
  filter(YEAR %in% c(1970, 2020)) |>
  select(
    race_eth,
    YEAR,
    count,
    weighted_count,
    ppbr = persons_per_bedroom
  )

# Pivot to wide
ppbr_wide <- ppbr_1970_2020 |>
  pivot_wider(
    names_from = YEAR,
    values_from = c(count, weighted_count, ppbr),
    names_sep = "_"
  )

# Compute changes
ppbr_final <- ppbr_wide |>
  mutate(
    change_ppbr = ppbr_2020 - ppbr_1970,
    pct_change_ppbr = 100 * change_ppbr / ppbr_1970
  ) |>
  arrange(race_eth)

ppbr_final