# fig03-single-mother-rent-burden-bedrooms-decade.R
#
# Violin plots of rent burden among single-mother renters by number of
# bedrooms, faceted by decade.
#
# Inputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)
#
# Outputs:
# - household-archetypes/output/figures/fig03-single-mother-rent-burden-bedrooms-decade.png
# - household-archetypes/output/figure-data/fig03-single-mother-rent-burden-bedrooms-decade.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb", read_only = TRUE)

single_mothers <- tbl(con, "single_mothers")

# --- rent burden by bedrooms and decade ---

rent_burden_by_bedrooms <- single_mothers |>
  filter(
    OWNERSHP == 2, # renters
    RELATE == 1,   # householders
    HHINCOME > 0
  ) |>
  mutate(
    cost_burden = (RENT * 12) / HHINCOME,
    YEAR = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    bedrooms_recode = pmin(BEDROOMS - 1L, 5L)
  ) |>
  filter(cost_burden <= 1) |>
  collect()

ggplot(rent_burden_by_bedrooms,
       aes(x = factor(bedrooms_recode), y = cost_burden, weight = PERWT)) +
  geom_violin() +
  facet_wrap(~YEAR) +
  labs(
    x = "Number of Bedrooms",
    y = "Rent Burden (Rent / Income)",
    title = "Rent Burden Among Single-Mother Renters by Bedrooms and Decade",
    caption = "Rent burdens greater than 1 are excluded."
  )

ggsave("household-archetypes/output/figures/fig03-single-mother-rent-burden-bedrooms-decade.png", last_plot())
write.csv(rent_burden_by_bedrooms, "household-archetypes/output/figure-data/fig03-single-mother-rent-burden-bedrooms-decade.csv", row.names = FALSE)

# --- same thing but as bars of the weighted mean ---

rent_burden_means <- rent_burden_by_bedrooms |>
  crosstab_mean(
    value = "cost_burden",
    wt_col = "PERWT",
    group_by = c("YEAR", "bedrooms_recode")
  )

ggplot(rent_burden_means,
       aes(x = factor(bedrooms_recode), y = weighted_mean)) +
  geom_col() +
  facet_wrap(~YEAR) +
  labs(
    x = "Number of Bedrooms",
    y = "Mean Rent Burden (Rent / Income)",
    title = "Mean Rent Burden Among Single-Mother Renters by Bedrooms and Decade",
    caption = "Rent burdens greater than 1 are excluded."
  )

ggsave("household-archetypes/output/figures/fig03b-single-mother-rent-burden-bedrooms-decade-means.png", last_plot())
write.csv(rent_burden_means, "household-archetypes/output/figure-data/fig03b-single-mother-rent-burden-bedrooms-decade-means.csv", row.names = FALSE)

# --- same data as a line chart: one line per bedroom count over time ---

ggplot(rent_burden_means,
       aes(x = YEAR, y = weighted_mean, color = factor(bedrooms_recode))) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Mean Rent Burden (Rent / Income)",
    color = "Bedrooms",
    title = "Mean Rent Burden Over Time Among Single-Mother Renters by Bedrooms",
    caption = "Rent burdens greater than 1 are excluded."
  )

ggsave("household-archetypes/output/figures/fig03c-single-mother-rent-burden-bedrooms-over-time.png", last_plot())

dbDisconnect(con)
