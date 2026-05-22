# fig03-housing-stock-by-build-cohort-2020.R
#
# Bar chart of the weighted occupied housing stock in 2020, by build cohort.
# Bars sum to the total number of owner- or renter-occupied households in 2020.
#
# Inputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/output/figures/fig03-housing-stock-by-build-cohort-2020.png
# - bedroom-allocation/output/figure-data/fig03-housing-stock-by-build-cohort-2020.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
devtools::load_all("../demographr")

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/households.duckdb",
  read_only = TRUE
)
households <- tbl(con, "households")

# --- weighted counts by cohort, 2020 only ---

cohort_counts <- households |>
  filter(decade == 2020L) |>
  crosstab_count(
    wt_col = "HHWT",
    group_by = "build_cohort"
  ) |>
  collect()

dbDisconnect(con)

cohort_order <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969",
  "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009",
  "2010 - 2019", "2020 onward"
)

cohort_counts <- cohort_counts |>
  filter(!is.na(build_cohort)) |>
  mutate(build_cohort = factor(build_cohort, levels = cohort_order))

fig <- ggplot(cohort_counts, aes(x = build_cohort, y = weighted_count)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Build Cohort",
    y = "Weighted Number of Households",
    title = "Occupied Housing Stock by Build Cohort, 2020"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "bedroom-allocation/output/figures/fig03-housing-stock-by-build-cohort-2020.png",
  plot = fig,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

write.csv(
  cohort_counts,
  "bedroom-allocation/output/figure-data/fig03-housing-stock-by-build-cohort-2020.csv",
  row.names = FALSE
)
