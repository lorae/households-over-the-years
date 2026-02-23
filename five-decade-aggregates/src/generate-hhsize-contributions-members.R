# generate-hhsize-contributions-members.R

households <- read_csv("five-decade-aggregates/output/raw/combined_cps_adults.csv")

households_diff <- households |>
  arrange(YEAR) |>
  mutate(
    hhsize_diff = hhsize - lag(hhsize),
    n_child_diff = n_child - lag(n_child),
    n_spouse_diff = n_spouse - lag(n_spouse),
    n_other_subfamily_members_diff = n_other_subfamily_members - lag(n_other_subfamily_members),
    decade_label = paste0(lag(YEAR), "-", YEAR)
  )  |>
  relocate(decade_label, .after = YEAR) |>
  mutate(
    components_sum = n_child_diff + n_spouse_diff + n_other_subfamily_members_diff
  ) # note: this components sum is a little different from the hhsize_diff because of single moms


write_csv(
  households_diff,
  "five-decade-aggregates/output/raw/hhsize_contributions_members.csv"
)