# scatter-crowding-vs-cost-state-tenure.R
#
# Four-panel scatter: state-level mean persons-per-bedroom vs mean annual
# housing cost (2020$), faceted by tenure (owner/renter) and year (1980/2020).
#
# Reads the aggregates produced by:
#   src/tables/generate-crowding-cost-state-tenure-decade.R
# State groups (IPUMS pre-1980 multi-state aggregates) and the 0 STATEFIP for
# unidentified are dropped here. 50 states + DC remain.

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(purrr)

# ----- Step 1: Load aggregates ----- #
ppbr <- read_csv(
  "five-decade-aggregates/output/raw/ppbr_state_tenure_decade.csv",
  show_col_types = FALSE
)

cost <- read_csv(
  "five-decade-aggregates/output/raw/housing_cost_state_tenure_decade.csv",
  show_col_types = FALSE
)

# ----- Step 2: 2-letter state abbreviations (50 states + DC) ----- #
state_abbrev <- tibble(
  state_name = c(datasets::state.name, "District of Columbia"),
  state_abb  = c(datasets::state.abb,  "DC")
)

# ----- Step 3: Join and filter to 1980 + 2020 ----- #
plot_data <- ppbr |>
  inner_join(cost, by = c("STATEFIP", "YEAR", "tenure", "state_name")) |>
  inner_join(state_abbrev, by = "state_name") |>
  filter(YEAR %in% c(1980L, 2020L)) |>
  mutate(
    tenure = factor(tenure, levels = c("owner", "renter"),
                    labels = c("Owners", "Renters")),
    YEAR_lab = factor(YEAR, levels = c(1980L, 2020L),
                      labels = c("1980", "2020"))
  )

# ----- Step 4: Per-panel OLS slope and R^2 ----- #
# Slope is rescaled to "ppbr per $10,000" for readability (cost is in 5-digit $).
fit_stats <- plot_data |>
  group_by(tenure, YEAR_lab) |>
  summarise(
    fit = list(lm(mean_ppbr ~ mean_annual_cost_2020)),
    .groups = "drop"
  ) |>
  mutate(
    slope_per_10k = map_dbl(fit, ~ coef(.x)[["mean_annual_cost_2020"]] * 1e4),
    r2            = map_dbl(fit, ~ summary(.x)$r.squared),
    label = sprintf("slope = %+0.3f per $10k\nR² = %0.2f",
                    slope_per_10k, r2)
  )

# ----- Step 5: Plot ----- #
p <- ggplot(plot_data,
            aes(x = mean_annual_cost_2020, y = mean_ppbr, label = state_abb)) +
  geom_point(size = 2, color = "black", alpha = 0.7) +
  geom_text(size = 2.6, vjust = -0.7, color = "black",
            check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8,
              formula = y ~ x) +
  geom_text(
    data = fit_stats,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05, vjust = 1.2,
    size = 3, color = "red",
    inherit.aes = FALSE
  ) +
  facet_grid(tenure ~ YEAR_lab, scales = "free_x") +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(
    title = "State-level crowding vs annual housing cost",
    x = "Mean annual housing cost (2020$)",
    y = "Mean persons per bedroom",
    caption = "Source: IPUMS USA (1980 1%; 2018-2022 ACS 5-year stand-in for 2020). Each state weighted equally in reported regression results."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.y = element_text(angle = 0)
  )

# ----- Step 5: Save ----- #
ggsave(
  "five-decade-aggregates/output/scatter-crowding-vs-cost-state-tenure.png",
  plot = p,
  width  = 2400,
  height = 1800,
  units  = "px",
  dpi    = 200
)
