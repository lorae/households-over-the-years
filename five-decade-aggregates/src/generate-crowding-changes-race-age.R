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
out_dir <- "five-decade-aggregates/output/raw"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# Persons per bedroom (ppbr)
# ----------------------------
ppbr_race_age_decade <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("race_eth", "age_bucket", "YEAR")
)


ppbr_race_age_decade_all <- crosstab_mean(
  data = base_data,
  value = "ppbr",
  wt_col = "PERWT",
  group_by = c("age_bucket", "YEAR")
) |>
  mutate(race_eth = "All")


ppbr_race_age_decade_combined <- bind_rows(
  ppbr_race_age_decade,
  ppbr_race_age_decade_all
) |>
  collect() |>
  clean_ipums_years("YEAR") |>
  arrange(YEAR, race_eth, age_bucket)

write_csv(ppbr_race_age_decade_combined,
          file.path(out_dir, "ppbr_race_age_decade.csv"))

# ----------------------------
# % crowded
# ----------------------------
crowded_race_age_decade <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "race_eth", "age_bucket", "YEAR"),
  percent_group_by = c("race_eth", "age_bucket", "YEAR")
)

crowded_race_age_decade_all <- crosstab_percent(
  data = base_data,
  wt_col = "PERWT",
  group_by = c("crowded", "age_bucket", "YEAR"),
  percent_group_by = c("age_bucket", "YEAR")
) |>
  mutate(race_eth = "All")

crowded_race_age_decade_combined <- bind_rows(
  crowded_race_age_decade,
  crowded_race_age_decade_all
) |>
  collect() |>
  filter(crowded) |>
  select(-crowded) |>
  clean_ipums_years("YEAR") |>
  arrange(YEAR, race_eth, age_bucket)

write_csv(
  crowded_race_age_decade_combined,
  file.path(out_dir, "crowded_race_age_decade.csv")
)


# ----------------------------
# Change in persons per bedroom (ppbr)
# ----------------------------
ppbr_race_age_change <- ppbr_race_age_decade_combined |>
  filter(YEAR %in% c(1970, 2020)) |>
  select(
    race_eth,
    age_bucket,
    YEAR,
    count,
    weighted_count,
    ppbr = weighted_mean
  ) |>
  pivot_wider(
    names_from = YEAR,
    values_from = c(count, weighted_count, ppbr),
    names_sep = "_"
  ) |>
  mutate(
    change_ppbr = ppbr_2020 - ppbr_1970,
    pct_change_ppbr = 100 * change_ppbr / ppbr_1970
  ) |>
  arrange(race_eth, age_bucket) |>
  filter(race_eth != "Multiracial") # does not exist in 1970

ppbr_race_age_change

write_csv(
  ppbr_race_age_change,
  file.path(out_dir, "ppbr_race_age_change_1970_2020.csv")
)

# ----------------------------
# Change in % crowded
# ----------------------------
crowded_race_age_change <- crowded_race_age_decade_combined |>
  filter(YEAR %in% c(1970, 2020)) |>
  select(
    race_eth,
    age_bucket,
    YEAR,
    count,
    weighted_count,
    pct_crowded = percent
  ) |>
  pivot_wider(
    names_from = YEAR,
    values_from = c(count, weighted_count, pct_crowded),
    names_sep = "_"
  ) |>
  mutate(
    change_pct_crowded = pct_crowded_2020 - pct_crowded_1970,
    pct_change_pct_crowded = 100 * change_pct_crowded / pct_crowded_1970
  ) |>
  arrange(race_eth, age_bucket) |>
  filter(race_eth != "Multiracial")  # does not exist in 1970

crowded_race_age_change

write_csv(
  crowded_race_age_change,
  file.path(out_dir, "crowded_race_age_change_1970_2020.csv")
)

# ----------------------------
# Household size (hhsize) by race × age × decade
# ----------------------------
hhsize_race_age_decade <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("race_eth", "age_bucket", "YEAR")
)

hhsize_race_age_decade_all <- crosstab_mean(
  data = base_data,
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("age_bucket", "YEAR")
) |>
  mutate(race_eth = "All")

hhsize_race_age_decade_combined <- bind_rows(
  hhsize_race_age_decade,
  hhsize_race_age_decade_all
) |>
  collect() |>
  clean_ipums_years("YEAR") |>
  arrange(YEAR, race_eth, age_bucket)

write_csv(
  hhsize_race_age_decade_combined,
  file.path(out_dir, "hhsize_race_age_decade.csv")
)

# ----------------------------
# Change in household size (hhsize)
# ----------------------------
hhsize_race_age_change <- hhsize_race_age_decade_combined |>
  filter(YEAR %in% c(1970, 2020)) |>
  select(
    race_eth,
    age_bucket,
    YEAR,
    count,
    weighted_count,
    hhsize = weighted_mean
  ) |>
  pivot_wider(
    names_from = YEAR,
    values_from = c(count, weighted_count, hhsize),
    names_sep = "_"
  ) |>
  mutate(
    change_hhsize = hhsize_2020 - hhsize_1970,
    pct_change_hhsize = 100 * change_hhsize / hhsize_1970
  ) |>
  arrange(race_eth, age_bucket) |>
  filter(race_eth != "Multiracial")

write_csv(
  hhsize_race_age_change,
  file.path(out_dir, "hhsize_race_age_change_1970_2020.csv")
)


dbDisconnect(con)