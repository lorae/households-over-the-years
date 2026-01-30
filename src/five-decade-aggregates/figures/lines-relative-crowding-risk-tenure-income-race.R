# lines-relative-crowding-risk-tenure-income-race.R

library(dplyr)
library(readr)
library(ggplot2)
library(patchwork)

# ----------------------------
# Read data
# ----------------------------
crowded_income <- read_csv(
  "output/five-decade-tables/raw/crowded_rel_risk_income_adult.csv",
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
# Plots
# ----------------------------
p_income <- ggplot(
  crowded_income,
  aes(x = YEAR, y = rel_risk, color = inctot_binned, linetype = inctot_binned, group = inctot_binned)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(crowded_income$YEAR))) +
  labs(x = "Year", y = "Relative crowding risk", color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_race <- ggplot(
  crowded_race,
  aes(x = YEAR, y = rel_risk, color = race_eth, linetype = race_eth, group = race_eth)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(crowded_race$YEAR))) +
  labs(x = "Year", y = "Relative crowding risk", color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_tenure <- ggplot(
  crowded_tenure,
  aes(x = YEAR, y = rel_risk, color = tenure, linetype = tenure, group = tenure)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(crowded_tenure$YEAR))) +
  labs(x = "Year", y = "Relative crowding risk", color = NULL, linetype = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

p_income
p_race
p_tenure
