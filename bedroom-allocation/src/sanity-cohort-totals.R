# sanity-cohort-totals.R
#
# Quick sanity check: sum the cohort × bedroom × decade weighted counts back
# up to a cohort × decade total, plot as one line per cohort over time.
# Should look like fig02 (same totals, just unstacked).
#
# Inputs:
# - bedroom-allocation/output/tables/table-cohort-bedroom-survival-1970-2020.csv

library(dplyr)
library(ggplot2)
library(readr)

survival <- read_csv(
  "bedroom-allocation/output/tables/table-cohort-bedroom-survival-1970-2020.csv"
)

cohort_order <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969",
  "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009",
  "2010 - 2019", "2020 onward"
)

cohort_totals <- survival |>
  group_by(build_cohort, decade) |>
  summarise(total_units = sum(weighted_count, na.rm = TRUE), .groups = "drop") |>
  mutate(build_cohort = factor(build_cohort, levels = cohort_order))

baseline_decade <- tibble(
  build_cohort    = factor(cohort_order, levels = cohort_order),
  baseline_decade = c(1970, 1970, 1970, 1970, 1980, 1990, 2000, 2010, 2020, NA_real_)
)

cohort_retention <- cohort_totals |>
  left_join(baseline_decade, by = "build_cohort") |>
  group_by(build_cohort) |>
  mutate(
    baseline_total = total_units[match(unique(baseline_decade), decade)][1],
    retention_pct  = if_else(
      !is.na(baseline_decade) & decade >= baseline_decade,
      total_units / baseline_total,
      NA_real_
    )
  ) |>
  ungroup()

fig_counts <- ggplot(cohort_totals, aes(x = decade, y = total_units, color = build_cohort)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Weighted occupied units",
    color = "Build cohort",
    title = "Sanity check: total occupied units by build cohort, 1970-2020"
  ) +
  theme_minimal()

fig_retention <- ggplot(
    cohort_retention |> filter(!is.na(retention_pct)),
    aes(x = decade, y = retention_pct, color = build_cohort)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey70") +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Retention (% of cohort baseline)",
    color = "Build cohort",
    title = "Sanity check: cohort retention rate, 1970-2020"
  ) +
  theme_minimal()

print(fig_counts)
print(fig_retention)
