# ----- Step 0: ACS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("writexl")

devtools::load_all("../demographr")

# ----- Step 1: Connect to DB ----- #
con <- dbConnect(
  duckdb::duckdb(),
  "data/five-decade-db/ipums_cps.duckdb"
)

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
# Decade-level means
# ================================

hhsize_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = "YEAR"
) |> arrange(YEAR)

nchild_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_children",
  wt_col = "ASECWT",
  group_by = "YEAR"
) |> arrange(YEAR)

spouse_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_spouse",
  wt_col = "ASECWT",
  group_by = "YEAR"
) |> arrange(YEAR)

nsubfamily_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_other_subfamilies",
  wt_col = "ASECWT",
  group_by = "YEAR"
) |> arrange(YEAR)

is_multifamily_decade_cps <- ipums_person |>
  mutate(
    is_multifamily = as.integer(n_other_subfamilies > 0)
  ) |>
  crosstab_mean(
    value = "is_multifamily",
    wt_col = "ASECWT",
    group_by = "YEAR"
  ) |>
  arrange(YEAR) |>
  rename(fraction_multifamily = weighted_mean)

# ================================
# Subfamily-weighted average size
# ================================

othersubfamilysize_decade_cps <- ipums_person |>
  filter(n_other_subfamilies > 0) |>
  mutate(subfamily_weight = n_other_subfamilies * ASECWT) |>
  group_by(YEAR) |>
  summarise(
    weighted_mean =
      sum(avg_other_subfamily_size * subfamily_weight, na.rm = TRUE) /
      sum(subfamily_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  collect() |>
  arrange(YEAR)

# ================================
# Combine into final table
# ================================

combined_cps_adults <- hhsize_decade_cps |>
  select(YEAR, weighted_count, count, hhsize = weighted_mean) |>
  left_join(
    nchild_decade_cps |> select(YEAR, n_child = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    spouse_decade_cps |> select(YEAR, n_spouse = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    nsubfamily_decade_cps |> select(YEAR, n_other_subfamily = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    othersubfamilysize_decade_cps |>
      select(YEAR, avg_other_subfamily_size = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    is_multifamily_decade_cps |>
      select(YEAR, fraction_multifamily),
    by = "YEAR"
  ) |>
  mutate(
    n_other_subfamily_members = n_other_subfamily * avg_other_subfamily_size,
    calculated_hhsize =
      1 + n_child + n_spouse + n_other_subfamily_members
  )

combined_cps_adults

readr::write_csv(
  combined_cps_adults,
  "five-decade-aggregates/output/raw/combined_cps_adults.csv"
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
  group_by = "YEAR"
) |> arrange(YEAR)

nsubfamily_decade_cps_all <- crosstab_mean(
  data = ipums_person_all,
  value = "n_other_subfamilies",
  wt_col = "ASECWT",
  group_by = "YEAR"
) |> arrange(YEAR)

is_multifamily_decade_cps_all <- ipums_person_all |>
  mutate(
    is_multifamily = as.integer(n_other_subfamilies > 0)
  ) |>
  crosstab_mean(
    value = "is_multifamily",
    wt_col = "ASECWT",
    group_by = "YEAR"
  ) |>
  arrange(YEAR) |>
  rename(fraction_multifamily = weighted_mean)

# ----------------
# Subfamily-weighted average size
# ----------------

othersubfamilysize_decade_cps_all <- ipums_person_all |>
  filter(n_other_subfamilies > 0) |>
  mutate(subfamily_weight = n_other_subfamilies * ASECWT) |>
  group_by(YEAR) |>
  summarise(
    weighted_mean =
      sum(avg_other_subfamily_size * subfamily_weight, na.rm = TRUE) /
      sum(subfamily_weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  collect() |>
  arrange(YEAR)

# ----------------
# Combine final table
# ----------------

combined_cps <- hhsize_decade_cps_all |>
  select(YEAR, weighted_count, count, hhsize = weighted_mean) |>
  left_join(
    nsubfamily_decade_cps_all |>
      select(YEAR, n_other_subfamily = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    othersubfamilysize_decade_cps_all |>
      select(YEAR, avg_other_subfamily_size = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    is_multifamily_decade_cps_all |>
      select(YEAR, fraction_multifamily),
    by = "YEAR"
  ) |>
  mutate(
    n_other_subfamily_members =
      n_other_subfamily * avg_other_subfamily_size
  )

combined_cps

readr::write_csv(
  combined_cps,
  "five-decade-aggregates/output/raw/combined_cps.csv"
)