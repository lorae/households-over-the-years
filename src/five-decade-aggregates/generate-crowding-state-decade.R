# ----- Step 0: ACS ----- #
library(dplyr)
library(duckdb)
library(dbplyr)
library(readr)
library(tidyr)
library(writexl)

devtools::load_all("../demographr")

# ================================
# Helpers: STATEFIP -> state name
# ================================

state_lookup <- function(include_pr = FALSE, include_groups = FALSE) {
  x <- tibble::tribble(
    ~STATEFIP, ~state_name,
    1L,  "Alabama",
    2L,  "Alaska",
    4L,  "Arizona",
    5L,  "Arkansas",
    6L,  "California",
    8L,  "Colorado",
    9L,  "Connecticut",
    10L, "Delaware",
    11L, "District of Columbia",
    12L, "Florida",
    13L, "Georgia",
    15L, "Hawaii",
    16L, "Idaho",
    17L, "Illinois",
    18L, "Indiana",
    19L, "Iowa",
    20L, "Kansas",
    21L, "Kentucky",
    22L, "Louisiana",
    23L, "Maine",
    24L, "Maryland",
    25L, "Massachusetts",
    26L, "Michigan",
    27L, "Minnesota",
    28L, "Mississippi",
    29L, "Missouri",
    30L, "Montana",
    31L, "Nebraska",
    32L, "Nevada",
    33L, "New Hampshire",
    34L, "New Jersey",
    35L, "New Mexico",
    36L, "New York",
    37L, "North Carolina",
    38L, "North Dakota",
    39L, "Ohio",
    40L, "Oklahoma",
    41L, "Oregon",
    42L, "Pennsylvania",
    44L, "Rhode Island",
    45L, "South Carolina",
    46L, "South Dakota",
    47L, "Tennessee",
    48L, "Texas",
    49L, "Utah",
    50L, "Vermont",
    51L, "Virginia",
    53L, "Washington",
    54L, "West Virginia",
    55L, "Wisconsin",
    56L, "Wyoming"
  )
  
  if (include_pr) {
    x <- bind_rows(x, tibble(STATEFIP = 72L, state_name = "Puerto Rico"))
  }
  
  if (include_groups) {
    x <- bind_rows(
      x,
      tibble::tribble(
        ~STATEFIP, ~state_name,
        61L, "Maine-New Hampshire-Vermont (group)",
        62L, "Massachusetts-Rhode Island (group)",
        63L, "MN-IA-MO-KS-NE-SD-ND (group)",
        64L, "Maryland-Delaware (group)",
        65L, "Montana-Idaho-Wyoming (group)",
        66L, "Utah-Nevada (group)",
        67L, "Arizona-New Mexico (group)",
        68L, "Alaska-Hawaii (group)",
        97L, "Overseas Military Installations",
        99L, "State not identified"
      )
    )
  }
  
  x
}

add_state_names <- function(df, statefip_col = "STATEFIP",
                            include_pr = FALSE, include_groups = FALSE) {
  lookup <- state_lookup(include_pr = include_pr, include_groups = include_groups)
  
  df |>
    mutate(STATEFIP = as.integer(.data[[statefip_col]])) |>
    left_join(lookup, by = "STATEFIP")
}

clean_ipums_years <- function(df, year_col = "YEAR") {
  df |>
    mutate(
      !!year_col := recode(
        as.integer(.data[[year_col]]),
        `2012` = 2010L,
        `2022` = 2020L
      )
    )
}

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))

# ----- Step 2: Outputs ----- #
out_dir <- "output/five-decade-tables/raw"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# Household size (NUMPREC)
# ----------------------------
hhsize_state_decade <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  clean_ipums_years("YEAR") |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, state_name)

write_csv(hhsize_state_decade,
          file.path(out_dir, "hhsize_state_decade.csv"))

# ----------------------------
# Bedrooms
# ----------------------------
bedroom_state_decade <- crosstab_mean(
  data = base_data,
  value = "bedroom",
  wt_col = "PERWT",
  group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  clean_ipums_years("YEAR") |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, state_name)

write_csv(bedroom_state_decade,
          file.path(out_dir, "bedroom_state_decade.csv"))

# ----------------------------
# Persons per bedroom (ppbr)
# ----------------------------
ppbr_state_decade <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  clean_ipums_years("YEAR") |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, state_name)

write_csv(ppbr_state_decade,
          file.path(out_dir, "ppbr_state_decade.csv"))


# ----------------------------
# % Crowded
# ----------------------------
crowded_state_decade <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "STATEFIP", "YEAR"),
  percent_group_by = c("STATEFIP", "YEAR")
) |>
  collect() |>
  filter(crowded) |>
  select(-crowded) |>
  clean_ipums_years("YEAR") |>
  add_state_names(include_groups = TRUE) |>
  arrange(YEAR, state_name) |>
  rename(crowded = percent)

write_csv(crowded_state_decade,
          file.path(out_dir, "crowded_state_decade.csv"))

dbDisconnect(con)

