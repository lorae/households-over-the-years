# fig02-single-mother-cost-burden-decade.R
#
# Distribution of housing cost burden among single-mother renters, by decade.
#
# Inputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)
#
# Outputs:
# - household-archetypes/output/figures/fig02-single-mother-cost-burden-decade.png
# - household-archetypes/output/figure-data/fig02-single-mother-cost-burden-decade.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb", read_only = TRUE)

single_mothers <- tbl(con, "single_mothers")

# --- calculate % of income spent on housing ---

pct_income_housing <- single_mothers |>
  filter(
    OWNERSHP == 2, # only renters
    RELATE == 1 # only the HOHs
    ) |>
  filter(HHINCOME > 0) |>
  mutate(
    cost_burden = (RENT * 12) / HHINCOME,
    cost_burden_over_1 = cost_burden > 1
  )

# --- weighted histogram of cost burden by decade ---

cost_burden_hist <- pct_income_housing |>
  filter(cost_burden <= 1) |>
  mutate(
    YEAR = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    cost_burden_bin = floor(cost_burden * 100) / 100  # 0.01-width bins
  ) |>
  crosstab_percent(
    wt_col = "PERWT",
    group_by = c("YEAR", "cost_burden_bin"),
    percent_group_by = "YEAR"
  )

cost_burden_mean <- pct_income_housing |>
  filter(cost_burden <= 1) |>
  mutate(YEAR = case_when(
    YEAR == 2012L ~ 2010L,
    YEAR == 2022L ~ 2020L,
    .default = YEAR
  )) |>
  crosstab_mean(
    value = "cost_burden",
    wt_col = "PERWT",
    group_by = "YEAR"
  )

cost_burden_mode <- cost_burden_hist |>
  filter(cost_burden_bin > 0) |>
  group_by(YEAR) |>
  slice_max(percent, n = 1)

ggplot(cost_burden_hist, aes(x = cost_burden_bin, y = percent)) +
  geom_col(width = 0.01) +
  geom_vline(data = cost_burden_mean, aes(xintercept = weighted_mean), color = "red") +
  geom_text(data = cost_burden_mean, aes(x = weighted_mean, y = Inf, label = paste0("mean = ", round(weighted_mean, 2))),
            vjust = 1.5, hjust = -0.1, color = "red", size = 3) +
  geom_vline(data = cost_burden_mode, aes(xintercept = cost_burden_bin), color = "blue") +
  geom_text(data = cost_burden_mode, aes(x = cost_burden_bin, y = Inf, label = paste0("mode = ", cost_burden_bin)),
            vjust = 3.5, hjust = -0.1, color = "blue", size = 3) +
  facet_wrap(~YEAR) +
  labs(
    x = "Housing Cost Burden (Rent / Income)",
    y = "% of Single-Mother Renters",
    title = "Distribution of Housing Cost Burden Among Single-Mother Renters",
    caption = "Rent burdens greater than 1 are excluded."
  )

ggsave("household-archetypes/output/figures/fig02a-single-mother-cost-burden-decade.png", last_plot())
write.csv(cost_burden_hist, "household-archetypes/output/figure-data/fig02-single-mother-cost-burden-decade.csv", row.names = FALSE)

# --- same thing for homeowners ---

pct_income_housing_owners <- single_mothers |>
  filter(
    OWNERSHP == 1, # only homeowners
    RELATE == 1    # only the HOHs
  ) |>
  filter(HHINCOME > 0) |>
  mutate(
    cost_burden = (OWNCOST * 12) / HHINCOME,
    cost_burden_over_1 = cost_burden > 1
  )

cost_burden_hist_owners <- pct_income_housing_owners |>
  filter(cost_burden <= 1) |>
  mutate(
    YEAR = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    cost_burden_bin = floor(cost_burden * 100) / 100
  ) |>
  crosstab_percent(
    wt_col = "PERWT",
    group_by = c("YEAR", "cost_burden_bin"),
    percent_group_by = "YEAR"
  )

cost_burden_mean_owners <- pct_income_housing_owners |>
  filter(cost_burden <= 1) |>
  mutate(YEAR = case_when(
    YEAR == 2012L ~ 2010L,
    YEAR == 2022L ~ 2020L,
    .default = YEAR
  )) |>
  crosstab_mean(
    value = "cost_burden",
    wt_col = "PERWT",
    group_by = "YEAR"
  )

cost_burden_mode_owners <- cost_burden_hist_owners |>
  filter(cost_burden_bin > 0) |>
  group_by(YEAR) |>
  slice_max(percent, n = 1)

ggplot(cost_burden_hist_owners, aes(x = cost_burden_bin, y = percent)) +
  geom_col(width = 0.01) +
  geom_vline(data = cost_burden_mean_owners, aes(xintercept = weighted_mean), color = "red") +
  geom_text(data = cost_burden_mean_owners, aes(x = weighted_mean, y = Inf, label = paste0("mean = ", round(weighted_mean, 2))),
            vjust = 1.5, hjust = -0.1, color = "red", size = 3) +
  geom_vline(data = cost_burden_mode_owners, aes(xintercept = cost_burden_bin), color = "blue") +
  geom_text(data = cost_burden_mode_owners, aes(x = cost_burden_bin, y = Inf, label = paste0("mode = ", cost_burden_bin)),
            vjust = 3.5, hjust = -0.1, color = "blue", size = 3) +
  facet_wrap(~YEAR) +
  labs(
    x = "Housing Cost Burden (Owner Cost / Income)",
    y = "% of Single-Mother Homeowners",
    title = "Distribution of Housing Cost Burden Among Single-Mother Homeowners",
    caption = "Cost burdens greater than 1 are excluded."
  )

ggsave("household-archetypes/output/figures/fig02b-single-mother-cost-burden-decade-owners.png", last_plot())
write.csv(cost_burden_hist_owners, "household-archetypes/output/figure-data/fig02b-single-mother-cost-burden-decade-owners.csv", row.names = FALSE)

dbDisconnect(con)
