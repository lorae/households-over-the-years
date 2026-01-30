library(dplyr)
library(readr)
library(tidyr)
library(writexl)

data_cps <- read_csv("output/five-decade-tables/raw/combined_cps.csv")
# Could be helpful later
# data_cps_adult <- read_csv("output/five-decade-tables/raw/combined_cps_adults.csv")
# data_acs <- read_csv("output/five-decade-tables/raw/hhsize_overall.csv")

data_cps <- read_csv("output/five-decade-tables/raw/combined_cps.csv")

out <- data_cps |>
  select(
    YEAR,
    fraction_multifamily,
    n_other_subfamily,
    avg_other_subfamily_size
  ) |>
  pivot_longer(
    cols = -YEAR,
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(YEAR = as.character(YEAR)) |>
  pivot_wider(
    names_from = YEAR,
    values_from = value
  )

write_xlsx(
  list(cps = out),
  "output/five-decade-tables/cps_subfamily_summary.xlsx"
)

