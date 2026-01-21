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
con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums_cps.duckdb")
ipums_person <- tbl(con, "ipums_person_with_subfamilies") |>
  filter(AGE >= 18) |> # only adults
  mutate(
    n_other_subfamilies = n_subfamilies - 1,
    avg_other_subfamily_size = if_else(n_other_subfamilies == 0, 
                                       NA_real_, 
                                       nonsubfamily_size / n_other_subfamilies)
  )

hhsize_decade_cps <- crosstab_mean(
  data = ipums_person, # no GQ variable?
  value = "NUMPREC",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |> arrange(YEAR)

nchild_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_children",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |> arrange(YEAR)

spouse_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_spouse",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |> arrange(YEAR)

nsubfamily_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "n_other_subfamilies",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |> arrange(YEAR)

othersubfamilysize_decade_cps <- crosstab_mean(
  data = ipums_person,
  value = "avg_other_subfamily_size",
  wt_col = "ASECWT",
  group_by = c("YEAR")
) |> arrange(YEAR)

hhsize_decade_cps
nchild_decade_cps
spouse_decade_cps
nsubfamily_decade_cps
othersubfamilysize_decade_cps


combined_cps <- hhsize_decade_cps |>
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
    nsubfamily_decade_cps |> select(YEAR, n_subfamily = weighted_mean),
    by = "YEAR"
  ) |>
  left_join(
    othersubfamilysize_decade_cps |> select(YEAR, subfamily_size = weighted_mean),
    by = "YEAR"
  ) |>
  mutate(
    calculated_hhsize = 1 + n_child + n_spouse + n_subfamily * subfamily_size
  )

combined_cps