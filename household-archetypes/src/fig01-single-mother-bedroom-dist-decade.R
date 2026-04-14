# fig01-single-mother-bedroom-dist-decade.R
#
# Plot bedroom distribution among single-mother households by decade.
#
# Inputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)
#
# Outputs:
# - household-archetypes/output/figures/fig01-single-mother-bedroom-dist-decade.png
# - household-archetypes/output/figure-data/fig01-single-mother-bedroom-dist-decade.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb", read_only = TRUE)

single_mothers <- tbl(con, "single_mothers")

# --- bedroom distribution by decade ---

bedrooms_by_decade <- single_mothers |>
  filter(RELATE == 1) |>
  mutate(
    YEAR = case_when(
      YEAR == 2012L ~ 2010L,
      YEAR == 2022L ~ 2020L,
      .default = YEAR
    ),
    bedrooms_recode = pmin(BEDROOMS - 1L, 5L)
  ) |>
  crosstab_percent(
    wt_col = "PERWT",
    group_by = c("YEAR", "bedrooms_recode"),
    percent_group_by = "YEAR"
  )

fig01 <- ggplot(bedrooms_by_decade, aes(x = bedrooms_recode, y = percent)) +
  geom_col() +
  facet_wrap(~YEAR) +
  labs(
    x = "Number of Bedrooms",
    y = "% of Single Mothers",
    title = "Bedroom Distribution Among Single-Mother Households"
  )

ggsave("household-archetypes/output/figures/fig01-single-mother-bedroom-dist-decade.png", fig01)
write.csv(bedrooms_by_decade, "household-archetypes/output/figure-data/fig01-single-mother-bedroom-dist-decade.csv", row.names = FALSE)

dbDisconnect(con)
