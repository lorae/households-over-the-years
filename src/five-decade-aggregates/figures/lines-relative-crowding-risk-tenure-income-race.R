# lines-relative-crowding-risk-tenure-income-race.R

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

out_dir <- "output/five-decade-tables"
# ----------------------------
# Read data
# ----------------------------
crowded_income <- read_csv(
  "output/five-decade-tables/raw/crowded_rel_risk_income.csv",
  show_col_types = FALSE
)

crowded_race <- read_csv(
  "output/five-decade-tables/raw/crowded_rel_risk_race.csv",
  show_col_types = FALSE
)

crowded_tenure <- read_csv(
  "output/five-decade-tables/raw/crowded_rel_risk_tenure.csv",
  show_col_types = FALSE
)

# ----------------------------
# Color specs + ordering
# ----------------------------
race_levels <- c(
  "White",
  "Multiracial",
  "Black",
  "Other",
  "AAPI",
  "AIAN",
  "Hispanic"
)

race_colors <- c(
  "White"        = "#03071e",
  "Multiracial" = "#6a8532",
  "Black"       = "#87a330",
  "Other"       = "#a1c349",
  "AAPI"        = "#f3c053",
  "AIAN"        = "#f9a03f",
  "Hispanic"    = "#eb5e28"
)

income_levels <- c(
  "$150,000 and greater",
  "$100,000 - $149,999",
  "$50,000 - $99,999",
  "less than $50,000"
)

income_colors <- c(
  "$150,000 and greater" = "#03071e",
  "$100,000 - $149,999" = "#87a330",
  "$50,000 - $99,999"   = "#f3c053",
  "less than $50,000"   = "#eb5e28"
)

tenure_levels <- c("owner", "renter")

tenure_colors <- c(
  "Owner-occupied"  = "#03071e",
  "Renter-occupied" = "#eb5e28"
)

# ----------------------------
# Plots
# ----------------------------
p_income <- crowded_income |>
  mutate(hhincome_2020_binned = factor(hhincome_2020_binned, levels = income_levels)) |>
  ggplot(aes(x = YEAR, y = rel_risk, color = hhincome_2020_binned, group = hhincome_2020_binned)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = income_colors) +
  scale_x_continuous(breaks = sort(unique(crowded_income$YEAR))) +
  labs(x = "Year", y = "Relative crowding risk", color = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_race <- crowded_race |>
  mutate(race_eth = factor(race_eth, levels = race_levels)) |>
  ggplot(aes(x = YEAR, y = rel_risk, color = race_eth, group = race_eth)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = race_colors) +
  scale_x_continuous(breaks = sort(unique(crowded_race$YEAR))) +
  labs(x = "Year", y = "Relative crowding risk", color = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_tenure <- crowded_tenure |>
  mutate(
    tenure = recode(
      tenure,
      owner  = "Owner-occupied",
      renter = "Renter-occupied"
    ),
    tenure = factor(
      tenure,
      levels = c("Owner-occupied", "Renter-occupied")
    )
  ) |>
  ggplot(aes(x = YEAR, y = rel_risk, color = tenure, group = tenure)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = tenure_colors) +
  scale_x_continuous(breaks = sort(unique(crowded_tenure$YEAR))) +
  labs(x = NULL, y = "Relative crowding risk", color = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(out_dir, "lines-relative-crowding-risk-tenure.png"),
  plot = p_tenure,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "lines-relative-crowding-risk-race.png"),
  plot = p_race,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "lines-relative-crowding-risk-income-adults.png"),
  plot = p_income,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300
)
