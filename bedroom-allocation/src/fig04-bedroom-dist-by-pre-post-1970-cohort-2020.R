# fig04-bedroom-dist-by-pre-post-1970-cohort-2020.R
#
# Bedroom-count distribution of the 2020 occupied housing stock, split by
# pre-1970 vs post-1970 build cohorts. Percentages sum to 100 within each
# cohort group. Uses the adjusted bedrooms variable (BEDROOMS - 1, capped at 5,
# so 0 = studio and 5 = 5+).
#
# Inputs:
# - bedroom-allocation/throughput/households.duckdb (table: households)
#
# Outputs:
# - bedroom-allocation/output/figures/fig04-bedroom-dist-by-pre-post-1970-cohort-2020.png
# - bedroom-allocation/output/figure-data/fig04-bedroom-dist-by-pre-post-1970-cohort-2020.csv

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

# --- 2020 only, drop unknown build cohort and BEDROOMS == 0 (IPUMS N/A → -1) ---

pre_1970_cohorts <- c(
  "1939 or earlier", "1940 - 1949", "1950 - 1959", "1960 - 1969"
)

fig_data <- households |>
  filter(
    decade == 2020L,
    !is.na(build_cohort),
    bedrooms_recode >= 0
  ) |>
  mutate(
    cohort_group = case_when(
      build_cohort %in% pre_1970_cohorts ~ "Pre-1970",
      .default = "Post-1970"
    )
  ) |>
  crosstab_percent(
    wt_col = "HHWT",
    group_by = c("cohort_group", "bedrooms_recode"),
    percent_group_by = "cohort_group"
  ) |>
  collect()

dbDisconnect(con)

fig_data <- fig_data |>
  mutate(cohort_group = factor(cohort_group, levels = c("Pre-1970", "Post-1970")))

d_pre  <- fig_data |> filter(cohort_group == "Pre-1970")
d_post <- fig_data |> filter(cohort_group == "Post-1970")

fig <- ggplot(mapping = aes(x = factor(bedrooms_recode), y = percent)) +
  geom_bar(
    data = d_pre,
    aes(fill = cohort_group, color = cohort_group),
    stat = "identity",
    alpha = 0.4,
    width = 0.9,
    size = 0.3
  ) +
  geom_bar(
    data = d_post,
    aes(fill = cohort_group, color = cohort_group),
    stat = "identity",
    alpha = 0.7,
    width = 0.6,
    size = 0.3
  ) +
  scale_fill_manual(values = c("Pre-1970" = "skyblue", "Post-1970" = "forestgreen")) +
  scale_color_manual(values = c("Pre-1970" = "skyblue", "Post-1970" = "forestgreen")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    x = "Number of Bedrooms (0 = studio, 5 = 5+)",
    y = NULL,
    fill = NULL,
    color = NULL,
    title = "Bedroom Distribution of 2020 Occupied Housing Stock",
    subtitle = "By pre-1970 vs post-1970 build cohort"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(
  "bedroom-allocation/output/figures/fig04-bedroom-dist-by-pre-post-1970-cohort-2020.png",
  plot = fig,
  width = 6,
  height = 5,
  units = "in",
  dpi = 300
)

write.csv(
  fig_data,
  "bedroom-allocation/output/figure-data/fig04-bedroom-dist-by-pre-post-1970-cohort-2020.csv",
  row.names = FALSE
)

# --- weighted mean bedrooms by cohort group ---
# Computed from the already-aggregated distribution:
# mean = sum(bedrooms_recode * share). Topcoded at 5 (5+ bin), so the true
# mean is slightly higher than what is reported here.
avg_bedrooms <- fig_data |>
  group_by(cohort_group) |>
  summarise(
    avg_bedrooms_topcoded = sum(bedrooms_recode * percent / 100),
    .groups = "drop"
  )

print(avg_bedrooms)

write.csv(
  avg_bedrooms,
  "bedroom-allocation/output/figure-data/fig04-avg-bedrooms-by-pre-post-1970-cohort-2020.csv",
  row.names = FALSE
)
