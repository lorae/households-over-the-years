# generate-hhsize-bedroom-contrib-decade.R
#
# Decomposes changes in persons per bedroom into contributions from household
# size vs. bedroom count changes, for each decade and for the full period,
# using three methods (equal effects, Das Gupta, log).
#
# Inputs:
# - five-decade-aggregates/output/raw/hhsize_decade_overall.csv
# - five-decade-aggregates/output/raw/bedroom_decade_overall.csv
#
# Outputs:
# - five-decade-aggregates/output/raw/decade_crowding_contributions.csv
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
  arrange(YEAR)

# ----------------------------
# Decomposition
# ----------------------------
# Takes one row per period with start/end hhsize and bedroom; returns one row
# per period x method with the share of the ppbr change from each component.
decompose_ppbr <- function(periods) {
  periods |>
    mutate(
      change_hhsize = hhsize_end - hhsize_start,
      change_bedroom = bedroom_end - bedroom_start,
      change_ppbr = hhsize_end / bedroom_end - hhsize_start / bedroom_start,
      # Equal effects: one added person counts the same as one removed bedroom
      equal_effects_hhsize = -change_hhsize / (change_bedroom - change_hhsize),
      equal_effects_bedroom = change_bedroom / (change_bedroom - change_hhsize),
      # Das Gupta: two-factor product decomposition, Das Gupta (1993), eqs. 2.1-2.9
      das_gupta_hhsize = change_hhsize * (1 / bedroom_start + 1 / bedroom_end) / 2 / change_ppbr,
      das_gupta_bedroom = (hhsize_start + hhsize_end) / 2 *
        (1 / bedroom_end - 1 / bedroom_start) / change_ppbr,
      # Log: ln(h/b) = ln(h) - ln(b), so the log change in ppbr splits exactly
      log_hhsize = log(hhsize_end / hhsize_start) /
        log((hhsize_end / bedroom_end) / (hhsize_start / bedroom_start)),
      log_bedroom = log(bedroom_start / bedroom_end) /
        log((hhsize_end / bedroom_end) / (hhsize_start / bedroom_start))
    ) |>
    select(
      period_label,
      starts_with("equal_effects_"),
      starts_with("das_gupta_"),
      starts_with("log_")
    ) |>
    pivot_longer(
      -period_label,
      names_to = c("method", ".value"),
      names_pattern = "(.*)_(hhsize|bedroom)"
    ) |>
    rename(
      contrib_percent_hhsize = hhsize,
      contrib_percent_bedroom = bedroom
    )
}

# ----------------------------
# Decade contributions
# ----------------------------
decade_contributions <- hhsize_bedrooms |>
  transmute(
    period_label = paste0(lag(YEAR), "-", YEAR),
    hhsize_start = lag(hhsize),
    hhsize_end = hhsize,
    bedroom_start = lag(bedroom),
    bedroom_end = bedroom
  ) |>
  filter(!is.na(hhsize_start)) |>
  decompose_ppbr()

write_csv(
  decade_contributions,
  file.path(out_dir, "decade_crowding_contributions.csv")
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
    bedroom_end = last(bedroom)
  ) |>
  decompose_ppbr()

write_csv(
  total_contributions,
  file.path(out_dir, "total_crowding_contributions.csv")
)
