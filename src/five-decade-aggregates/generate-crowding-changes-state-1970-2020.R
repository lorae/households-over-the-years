# generate-crowding-changes-state-1970-2020.R
library(dplyr)
library(readr)
library(tidyr)

out_dir <- "output/five-decade-tables/raw"

# ----------------------------
# Helper to build change tables
# ----------------------------
build_change_table <- function(df, value_name) {
  df |>
    filter(YEAR %in% c(1970, 2020)) |>
    select(
      STATEFIP,
      state_name,
      YEAR,
      count,
      weighted_count,
      value = weighted_mean
    ) |>
    pivot_wider(
      names_from = YEAR,
      values_from = c(count, weighted_count, value),
      names_glue = "{.value}_{YEAR}"
    ) |>
    mutate(
      change = value_2020 - value_1970,
      pct_change = 100 * (value_2020 - value_1970) / value_1970
    ) |>
    rename(
      !!paste0(value_name, "_1970") := value_1970,
      !!paste0(value_name, "_2020") := value_2020,
      !!paste0("change_", value_name) := change,
      !!paste0("pct_change_", value_name) := pct_change
    ) |>
    arrange(state_name)
}

# ----------------------------
# Household size
# ----------------------------
hhsize_state_decade <- read_csv(
  file.path(out_dir, "hhsize_state_decade.csv"),
  show_col_types = FALSE
)

hhsize_change <- build_change_table(hhsize_state_decade, "hhsize")

write_csv(
  hhsize_change,
  file.path(out_dir, "hhsize_state_change_1970_2020.csv")
)

# ----------------------------
# Bedrooms
# ----------------------------
bedroom_state_decade <- read_csv(
  file.path(out_dir, "bedroom_state_decade.csv"),
  show_col_types = FALSE
)

bedroom_change <- build_change_table(bedroom_state_decade, "bedroom")

write_csv(
  bedroom_change,
  file.path(out_dir, "bedroom_state_change_1970_2020.csv")
)

# ----------------------------
# Persons per bedroom (ppbr)
# ----------------------------
ppbr_state_decade <- read_csv(
  file.path(out_dir, "ppbr_state_decade.csv"),
  show_col_types = FALSE
)

ppbr_change <- build_change_table(ppbr_state_decade, "ppbr")

write_csv(
  ppbr_change,
  file.path(out_dir, "ppbr_state_change_1970_2020.csv")
)

# ----------------------------
# % Crowded
# ----------------------------
crowded_state_decade <- read_csv(
  file.path(out_dir, "crowded_state_decade.csv"),
  show_col_types = FALSE
)

crowded_change <- build_change_table(ppbr_state_decade, "percent_crowded")

write_csv(
  ppbr_change,
  file.path(out_dir, "crowded_state_change_1970_2020.csv")
)