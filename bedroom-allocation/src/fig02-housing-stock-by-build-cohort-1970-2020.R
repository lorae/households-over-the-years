# fig02-housing-stock-by-build-cohort-1970-2020.R
#
# Sand chart of the weighted occupied housing stock by build cohort across
# survey decades. Retention % labels at the right edge show the 2020 weighted
# count as a share of the earliest decade in which the cohort is fully observed.
#
# Inputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/output/figures/fig02-housing-stock-by-build-cohort-1970-2020.png
# - bedroom-allocation/output/figure-data/fig02-housing-stock-by-build-cohort-1970-2020.csv

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

# --- weighted counts by decade and cohort ---

cohort_counts <- households |>
  crosstab_count(
    wt_col = "HHWT",
    group_by = c("decade", "build_cohort")
  ) |>
  collect()

dbDisconnect(con)

cohort_order <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969",
  "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009",
  "2010 - 2019", "2020 onward"
)

cohort_counts <- cohort_counts |>
  mutate(build_cohort = factor(build_cohort, levels = rev(cohort_order)))

# Retention %: 2020 weighted count / count in the first decade the cohort is
# *fully* observed. Using the first *any* observation would give a partial
# count in the denominator (e.g. 2010-2019 homes sampled in 2010 are only
# ~1 year of construction), inflating the ratio.
# "2020 onward" is never fully observed in this data range, so it gets no label.
baseline_decade <- tibble(
  build_cohort = factor(cohort_order, levels = rev(cohort_order)),
  baseline_decade = c(1970, 1970, 1970, 1970, 1980, 1990, 2000, 2010, 2020, NA_real_)
)

retention_labels <- cohort_counts |>
  inner_join(baseline_decade, by = "build_cohort") |>
  filter(decade == baseline_decade) |>
  select(build_cohort, first_count = weighted_count) |>
  inner_join(
    cohort_counts |>
      filter(decade == 2020) |>
      select(build_cohort, last_count = weighted_count),
    by = "build_cohort"
  ) |>
  mutate(
    retention_pct = paste0(round(last_count / first_count * 100), "%")
  )

# Label y-positions must match ggplot's stacking order
# (factor level order, first level on bottom). inner_join drops "2020 onward".
label_positions <- cohort_counts |>
  filter(decade == 2020) |>
  arrange(desc(build_cohort)) |>
  mutate(
    ymax = cumsum(weighted_count),
    ymin = ymax - weighted_count,
    ymid = (ymin + ymax) / 2
  ) |>
  inner_join(retention_labels, by = "build_cohort")

fig <- ggplot(cohort_counts, aes(x = decade, y = weighted_count, fill = build_cohort)) +
  geom_area() +
  geom_text(
    data = label_positions,
    aes(x = 2021, y = ymid, label = retention_pct),
    hjust = 0, size = 2.5, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = c(1970, 1980, 1990, 2000, 2010, 2020),
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Survey Year",
    y = "Weighted Number of Households",
    fill = "Build Cohort",
    title = "Occupied Housing Stock by Build Cohort, 1970-2020"
  ) +
  theme_minimal()

ggsave(
  "bedroom-allocation/output/figures/fig02-housing-stock-by-build-cohort-1970-2020.png",
  plot = fig,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

write.csv(
  cohort_counts,
  "bedroom-allocation/output/figure-data/fig02-housing-stock-by-build-cohort-1970-2020.csv",
  row.names = FALSE
)
