# hhsize-buckets-race-decade.csv

# ----- Step 0: Config ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("writexl")

devtools::load_all("../demographr")

# ================================
# Helpers
# ================================

clean_years <- function(df) {
  df |>
    mutate(
      YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)
    )
}


clean_hhsize_output <- function(df, group_var = NULL, group_levels = NULL) {
  out <- df |>
    clean_years() |>
    rename(hhsize = weighted_mean)
  
  if (!is.null(group_var)) {
    out <- out |>
      mutate(
        !!group_var := factor(.data[[group_var]], levels = group_levels)
      ) |>
      arrange(YEAR, !!sym(group_var))
  } else {
    out <- out |> arrange(YEAR)
  }
  
  out
}

years <- c(1970, 1980, 1990, 2000, 2010, 2020)

race_levels <- c(
  "AIAN", "AAPI", "Black", "Hispanic",
  "White", "Multiracial", "Other"
)


# ----- Step 1: Connect to DB ----- #
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums.duckdb")

ipums_person <- tbl(con, "ipums_person") |>
  mutate(crowded = ppbr > 2)

base_data <- ipums_person |> filter(GQ %in% c(0, 1, 2))



# ----- Step 2: Compute table ----- #
hhsize_topcode <- 8

hhsize_decade_race_groups <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("NUMPREC", "race_eth", "YEAR"),
  percent_group_by = c("race_eth", "YEAR")
) 

hhsize_decade_all_groups <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("NUMPREC", "YEAR"),
  percent_group_by = c("YEAR")
) |>
  mutate(race_eth = "All")

hhsize_decade_race <- bind_rows(
  hhsize_decade_race_groups,
  hhsize_decade_all_groups
) |>
  # Topcode household size
  mutate(
    NUMPREC = if_else(NUMPREC >= hhsize_topcode,
                      hhsize_topcode,
                      NUMPREC)
  ) |>
  group_by(race_eth, YEAR, NUMPREC) |>
  summarise(
    percent = sum(percent),
    count = sum(count),
    .groups = "drop"
  ) |>
  # Relabel ACS years using midpoint
  clean_years() |>
  mutate(
    race_eth = factor(race_eth,
                      levels = c(race_levels, "All"))
  ) |>
  arrange(YEAR, race_eth, NUMPREC)

write_csv(
  hhsize_decade_race,
  "five-decade-aggregates/output/raw/hhsize-buckets-race-decade.csv"
)
