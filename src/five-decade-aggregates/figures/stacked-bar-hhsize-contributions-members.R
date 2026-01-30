# stacked-bar-hhsize-contributions-members

library(dplyr)
library(tidyr)
library(ggplot2)

hhsize_contrib <- read_csv(
  "output/five-decade-tables/raw/hhsize_contributions_members.csv",
  show_col_types = FALSE
)

hhsize_contrib |>
  filter(!is.na(decade_label), decade_label != "NA-1970") |>
  select(
    decade_label,
    n_child_diff,
    n_spouse_diff,
    n_other_subfamily_members_diff
  ) |>
  pivot_longer(
    cols = -decade_label,
    names_to = "component",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(
    component = recode(
      component,
      n_child_diff = "Change in children",
      n_spouse_diff = "Change in spouses",
      n_other_subfamily_members_diff = "Change in other household members"
    )
  ) |>
  ggplot(aes(x = decade_label, y = value, fill = component)) +
  geom_col() +
  labs(
    x = "Decade",
    y = "Change in household size (persons)",
    fill = "Component"
  )
