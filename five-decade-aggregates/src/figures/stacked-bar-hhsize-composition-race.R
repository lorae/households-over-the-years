# stacked-bar-hhsize-composition-race.R
#
# Stacked bar chart showing household size composition (reference person,
# spouse, children, other subfamily members) for Black, White, and Hispanic
# households in 1970 and 2020.
#
# Inputs:
# - five-decade-aggregates/output/raw/combined_cps_adults_race.csv
#
# Outputs:
# - five-decade-aggregates/output/stacked-bar-hhsize-composition-race.png

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

combined <- read_csv(
  "five-decade-aggregates/output/raw/combined_cps_adults_race.csv",
  show_col_types = FALSE
)

plot_data <- combined |>
  filter(
    race_eth %in% c("Black", "White", "Hispanic"),
    (race_eth != "Hispanic" & YEAR %in% c(1970, 2020)) |
      (race_eth == "Hispanic" & YEAR %in% c(1980, 2020))
  ) |>
  mutate(
    reference_person = 1,
    other_members = n_other_subfamily_members
  ) |>
  select(YEAR, race_eth, reference_person, n_spouse, n_child, other_members) |>
  pivot_longer(
    cols = c(reference_person, n_spouse, n_child, other_members),
    names_to = "component",
    values_to = "value"
  ) |>
  mutate(
    component = recode(
      component,
      reference_person = "Reference person",
      n_spouse = "Spouse",
      n_child = "Children",
      other_members = "Other household members"
    ),
    component = factor(
      component,
      levels = c(
        "Other household members",
        "Children",
        "Spouse",
        "Reference person"
      )
    ),
    bar_label = paste(race_eth, YEAR),
    bar_label = factor(
      bar_label,
      levels = c(
        "Black 1970", "Black 2020",
        "White 1970", "White 2020",
        "Hispanic 1980", "Hispanic 2020"
      )
    )
  )

p <- plot_data |>
  filter(!is.na(bar_label)) |>
  ggplot(aes(x = bar_label, y = value, fill = component)) +
  geom_col(color = "black", linewidth = 0.25) +
  scale_fill_manual(
    values = c(
      "Reference person" = "#a8dadc",
      "Spouse" = "#fcca46",
      "Children" = "#fe7f2d",
      "Other household members" = "#233d4d"
    )
  ) +
  labs(
    x = NULL,
    y = "Average household size (persons)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "five-decade-aggregates/output/stacked-bar-hhsize-composition-race.png",
  plot = p,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 300
)
