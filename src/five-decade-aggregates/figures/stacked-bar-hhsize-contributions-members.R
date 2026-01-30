# stacked-bar-hhsize-contributions-members

library(dplyr)
library(tidyr)
library(ggplot2)

hhsize_contrib <- read_csv(
  "output/five-decade-tables/raw/hhsize_contributions_members.csv",
  show_col_types = FALSE
)

p <- hhsize_contrib |>
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
    ),
    component = factor(
      component,
      levels = c(
        "Change in children",
        "Change in spouses",
        "Change in other household members"
      )
    )
  ) |>
  ggplot(aes(x = decade_label, y = value, fill = component)) +
  geom_col(color = "black", linewidth = 0.25) +
  scale_fill_manual(
    values = c(
      "Change in children" = "#fe7f2d",
      "Change in spouses" = "#fcca46",
      "Change in other household members" = "#233d4d"
    )
  ) +
  labs(
    x = "Decade",
    y = "Change in household size (persons)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
    legend.position = "bottom"
  )

ggsave(
  filename = "output/five-decade-tables/stacked-bar-hhsize-contributions-members.png",
  plot = p,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300
)

