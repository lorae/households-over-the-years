# generate-hhsize-bedroom-contrib-decade.R
#
# Decomposes intra-decade changes in persons per bedroom into contributions
# from household size vs. bedroom count changes.
#
# Inputs:
# - five-decade-aggregates/output/raw/hhsize_decade_overall.csv
# - five-decade-aggregates/output/raw/bedroom_decade_overall.csv
#
# Outputs:
# - five-decade-aggregates/output/raw/intra_decade_crowding_contributions.csv
# - five-decade-aggregates/output/raw/total_crowding_contributions.csv
#
library(dplyr)
library(readr)
library(tidyr)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "five-decade-aggregates/output/raw"
out_dir  <- "five-decade-aggregates/output/raw"

hhsize   <- read_csv(file.path(data_dir, "hhsize_decade_overall.csv"), show_col_types = FALSE)
bedrooms <- read_csv(file.path(data_dir, "bedroom_decade_overall.csv"), show_col_types = FALSE)
  
hhsize_bedrooms <- hhsize |>
  left_join(
    bedrooms |> select(YEAR, bedroom),
    by = "YEAR"
  ) |>
  arrange(YEAR) |>
  mutate(
    decade_label = if_else(
      is.na(lag(YEAR)),
      NA_character_,
      paste0(lag(YEAR), "-", YEAR)
    ),
    change_hhsize = hhsize - lag(hhsize),
    change_bedroom = bedroom - lag(bedroom)
  ) |>
  relocate(decade_label, .after = YEAR)|>
  mutate(
    contrib_percent_hhsize = -change_hhsize / (change_bedroom - change_hhsize),
    contrib_percent_bedroom = change_bedroom / (change_bedroom - change_hhsize)
  )

write_csv(
  hhsize_bedrooms,
  file.path(out_dir, "intra_decade_crowding_contributions.csv")
)

# ----------------------------
# Total contributions, first to last year
# ----------------------------
total_contributions <- hhsize_bedrooms |>
  summarise(
    period_label = paste0(first(YEAR), "-", last(YEAR)),
    hhsize_start = first(hhsize),
    hhsize_end = last(hhsize),
    bedroom_start = first(bedroom),
    bedroom_end = last(bedroom),
    change_hhsize = hhsize_end - hhsize_start,
    change_bedroom = bedroom_end - bedroom_start
  ) |>
  mutate(
    # Equal effects: one added person counts the same as one removed bedroom
    equal_effects_hhsize = -change_hhsize / (change_bedroom - change_hhsize),
    equal_effects_bedroom = change_bedroom / (change_bedroom - change_hhsize)
  ) |>
  select(period_label, starts_with("equal_effects_")) |>
  pivot_longer(
    -period_label,
    names_to = c("method", ".value"),
    names_pattern = "(.*)_(hhsize|bedroom)"
  ) |>
  rename(
    contrib_percent_hhsize = hhsize,
    contrib_percent_bedroom = bedroom
  )

write_csv(
  total_contributions,
  file.path(out_dir, "total_crowding_contributions.csv")
)