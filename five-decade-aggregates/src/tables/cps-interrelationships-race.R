# cps-interrelationships-race.R
#
# Aggregates CPS subfamily structure into decade-level summary statistics
# by race/ethnicity: household size, children, spouses, subfamily counts,
# and multifamily rates. Produces tables for adults-only and all-persons
# perspectives.
#
# Inputs:
# - data/five-decade-db/ipums_cps.duckdb (table: ipums_person_with_subfamilies_over18)
# - ../demographr (sibling package)
#
# Outputs:
# - five-decade-aggregates/output/raw/combined_cps_adults_race.csv
# - five-decade-aggregates/output/raw/combined_cps_race.csv
#
#
library("dplyr")
library("duckdb")
library("dbplyr")
library("readr")

devtools::load_all("../demographr")

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(
  duckdb::duckdb(),
  "data/five-decade-db/ipums_cps.duckdb"
)

group_cols <- c("YEAR", "race_eth")

ipums_person <- tbl(con, "ipums_person_with_subfamilies_over18") |>
  filter(AGE >= 18) |>  # adults only
  mutate(
    n_other_subfamilies = n_subfamilies - 1,
    avg_other_subfamily_size = if_else(
      n_other_subfamilies == 0,
      NA_real_,
      nonsubfamily_size / n_other_subfamilies
    )
  )

# ================================
# Decade-level means by race/eth
# ================================

hhsize_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

nchild_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_children",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

spouse_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_spouse",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

nsubfamily_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_other_subfamilies",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

is_multifamily_decade_cps <- ipums_person |>
  mutate(
    is_multifamily = as.integer(n_other_subfamilies > 0)
  ) |>
  crosstab_mean(
    value = "is_multifamily",
    wt_col = "ASECWT",
    group_by = group_cols
  ) |>
  arrange(YEAR, race_eth) |>
  rename(fraction_multifamily = weighted_mean)

# ================================
# Subfamily-weighted average size
# ================================

othersubfamilysize_decade_cps <- ipums_person |>
  filter(n_other_subfamilies > 0) |>
  mutate(subfamily_weight = n_other_subfamilies * ASECWT) |>
  group_by(YEAR, race_eth) |>
  summarise(
    weighted_mean =
      sum(avg_other_subfamily_size * subfamily_weight, na.rm = TRUE) /
      sum(subfamily_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  collect() |>
  arrange(YEAR, race_eth)

# ================================
# Combine into final table
# ================================

join_cols <- c("YEAR", "race_eth")

combined_cps_adults_race <- hhsize_decade_cps |>
  select(YEAR, race_eth, weighted_count, count, hhsize = weighted_mean) |>
  left_join(
    nchild_decade_cps |> select(YEAR, race_eth, n_child = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    spouse_decade_cps |> select(YEAR, race_eth, n_spouse = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    nsubfamily_decade_cps |> select(YEAR, race_eth, n_other_subfamily = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    othersubfamilysize_decade_cps |>
      select(YEAR, race_eth, avg_other_subfamily_size = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    is_multifamily_decade_cps |>
      select(YEAR, race_eth, fraction_multifamily),
    by = join_cols
  ) |>
  mutate(
    n_other_subfamily_members = n_other_subfamily * avg_other_subfamily_size,
    calculated_hhsize =
      1 + n_child + n_spouse + n_other_subfamily_members
  )

combined_cps_adults_race

readr::write_csv(
  combined_cps_adults_race,
  "five-decade-aggregates/output/raw/combined_cps_adults_race.csv"
)

# ================================
# CPS including children
# ================================

ipums_person_all <- tbl(con, "ipums_person_with_subfamilies_over18") |>
  # IMPORTANT: no AGE filter
  mutate(
    n_other_subfamilies = n_subfamilies - 1,
    avg_other_subfamily_size = if_else(
      n_other_subfamilies == 0,
      NA_real_,
      nonsubfamily_size / n_other_subfamilies
    )
  )

# ----------------
# Decade-level means
# ----------------

hhsize_decade_cps_all <- crosstab_mean(
  data = ipums_person_all,
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

nsubfamily_decade_cps_all <- crosstab_mean(
  data = ipums_person_all,
  value = "n_other_subfamilies",
  wt_col = "ASECWT",
  group_by = group_cols
) |> arrange(YEAR, race_eth)

is_multifamily_decade_cps_all <- ipums_person_all |>
  mutate(
    is_multifamily = as.integer(n_other_subfamilies > 0)
  ) |>
  crosstab_mean(
    value = "is_multifamily",
    wt_col = "ASECWT",
    group_by = group_cols
  ) |>
  arrange(YEAR, race_eth) |>
  rename(fraction_multifamily = weighted_mean)

# ----------------
# Subfamily-weighted average size
# ----------------

othersubfamilysize_decade_cps_all <- ipums_person_all |>
  filter(n_other_subfamilies > 0) |>
  mutate(subfamily_weight = n_other_subfamilies * ASECWT) |>
  group_by(YEAR, race_eth) |>
  summarise(
    weighted_mean =
      sum(avg_other_subfamily_size * subfamily_weight, na.rm = TRUE) /
      sum(subfamily_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  collect() |>
  arrange(YEAR, race_eth)

# ----------------
# Combine final table
# ----------------

combined_cps_race <- hhsize_decade_cps_all |>
  select(YEAR, race_eth, weighted_count, count, hhsize = weighted_mean) |>
  left_join(
    nsubfamily_decade_cps_all |>
      select(YEAR, race_eth, n_other_subfamily = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    othersubfamilysize_decade_cps_all |>
      select(YEAR, race_eth, avg_other_subfamily_size = weighted_mean),
    by = join_cols
  ) |>
  left_join(
    is_multifamily_decade_cps_all |>
      select(YEAR, race_eth, fraction_multifamily),
    by = join_cols
  ) |>
  mutate(
    n_other_subfamily_members =
      n_other_subfamily * avg_other_subfamily_size
  )

combined_cps_race

readr::write_csv(
  combined_cps_race,
  "five-decade-aggregates/output/raw/combined_cps_race.csv"
)

dbDisconnect(con)
