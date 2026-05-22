# fig05-cohort-bedroom-survival-1970-2020.R
#
# Exploratory plots for the cohort-bedroom survival analysis. Reads the
# precomputed survival table and prints four plots in succession:
#
#   1. Cohort totals — weighted occupied units summed across bedrooms.
#   2. Cohort retention — same totals indexed to each cohort's baseline decade.
#   3. Cohort × bedroom retention — faceted by cohort, one line per bedroom.
#   4. Cohort × bedroom counts — same facets and colors, in absolute terms.
#
# All plots drop the "2020 onward" cohort (no fully-observed decade) and drop
# the 2010 partial observation of the 2010-2019 cohort (only ~1 year of
# construction is sampled there).
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

baseline_decade <- tibble(
  build_cohort    = cohort_order,
  baseline_decade = c(1970, 1970, 1970, 1970, 1980, 1990, 2000, 2010, 2020, NA_real_)
)

# Warm-to-cool pastel palette, first 6 of 7 (drop lavender-blue).
palette_6 <- c("#ff8fc8", "#ffa489", "#ffb84a", "#b5c984", "#6bd9bd", "#83daf1")

# --- cohort totals (sum across bedrooms) for sanity plots ---

cohort_totals <- survival |>
  group_by(build_cohort, decade) |>
  summarise(total_units = sum(weighted_count, na.rm = TRUE), .groups = "drop") |>
  left_join(baseline_decade, by = "build_cohort") |>
  filter(!is.na(baseline_decade), decade >= baseline_decade) |>
  group_by(build_cohort) |>
  mutate(
    baseline_total = total_units[decade == baseline_decade][1],
    retention_pct  = total_units / baseline_total
  ) |>
  ungroup() |>
  mutate(build_cohort = factor(build_cohort, levels = cohort_order))

# --- per-bedroom data for the faceted plots (1960s through 2000s only) ---

cohort_keep <- c(
  "1960 - 1969", "1970 - 1979", "1980 - 1989", "1990 - 1999", "2000 - 2009"
)

facet_data <- survival |>
  filter(build_cohort %in% cohort_keep, !is.na(retention_pct)) |>
  mutate(
    build_cohort = factor(build_cohort, levels = cohort_keep),
    bedrooms = factor(
      bedrooms_recode,
      levels = 0:5,
      labels = c("0", "1", "2", "3", "4", "5+")
    )
  )

# --- plots ---

fig_totals_counts <- ggplot(cohort_totals, aes(x = decade, y = total_units, color = build_cohort)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Weighted occupied units",
    color = "Build cohort",
    title = "Total occupied units by build cohort, 1970-2020"
  ) +
  theme_minimal()

fig_totals_retention <- ggplot(cohort_totals, aes(x = decade, y = retention_pct, color = build_cohort)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey70") +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Retention (% of cohort baseline)",
    color = "Build cohort",
    title = "Cohort retention rate, 1970-2020"
  ) +
  theme_minimal()

fig_facet_retention <- ggplot(facet_data, aes(x = decade, y = retention_pct, color = bedrooms)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey70") +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ build_cohort, ncol = 3) +
  scale_color_manual(values = palette_6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Retention (% of cohort baseline)",
    color = "Bedrooms",
    title = "Cohort Bedroom Survival: occupied housing units by build cohort",
    subtitle = "Each cohort indexed to 100% in its first fully-observed decade"
  ) +
  theme_minimal()

fig_facet_counts <- ggplot(facet_data, aes(x = decade, y = weighted_count, color = bedrooms)) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ build_cohort, ncol = 3) +
  scale_color_manual(values = palette_6) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  labs(
    x = "Survey Year",
    y = "Weighted occupied units",
    color = "Bedrooms",
    title = "Cohort Bedroom Counts: occupied housing units by build cohort",
    subtitle = "Same facets and colors as the retention plot, in absolute terms"
  ) +
  theme_minimal()

print(fig_totals_counts)
print(fig_totals_retention)
print(fig_facet_retention)
print(fig_facet_counts)
