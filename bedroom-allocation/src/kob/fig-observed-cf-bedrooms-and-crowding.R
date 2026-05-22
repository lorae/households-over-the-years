# fig-observed-cf-bedrooms-and-crowding.R
#
# Two-panel figure:
#   Left panel  = mean bedrooms per HH
#   Right panel = crowding (persons per bedroom)
# Each panel shows 1970 observed, 2020 observed, and the 2020 counterfactual
# (1970 preferences applied to 2020 population).
#
# Bedroom values computed from kob_output + microdata.
# Crowding values taken from compute-actual-crowding.R and
# compute-cf-crowding-2020.R outputs (hardcoded for now).

library(dplyr)
library(dbplyr)
library(duckdb)
library(DBI)
library(ggplot2)
library(scales)
library(tibble)
library(patchwork)

# ----- Step 1: Bedroom values -----
kob_output <- readRDS("bedroom-allocation/throughput/kob_output.rds")
total_e    <- sum(kob_output$e, na.rm = TRUE)

con <- dbConnect(
  duckdb::duckdb(),
  "bedroom-allocation/throughput/kob-households.duckdb",
  read_only = TRUE
)
observed <- tbl(con, "households") |>
  filter(
    !is.na(bedrooms_recode),
    !is.na(hoh_age), !is.na(hoh_race_eth), !is.na(hoh_educ_bucket),
    !is.na(hoh_us_born), !is.na(hoh_sex), !is.na(tenure),
    !is.na(region4), !is.na(n_children_under_18), !is.na(n_adults),
    !is.na(hhincome_2020_harmonized)
  ) |>
  group_by(decade) |>
  summarise(mean_bedrooms = sum(bedrooms_recode * HHWT) / sum(HHWT),
            .groups = "drop") |>
  collect()
dbDisconnect(con)

bedroom_1970_obs <- observed |> filter(decade == 1970) |> pull(mean_bedrooms)
bedroom_2020_obs <- observed |> filter(decade == 2020) |> pull(mean_bedrooms)
bedroom_2020_cf  <- bedroom_1970_obs + total_e

# ----- Step 2: Crowding values -----
# From compute-actual-crowding.R and compute-cf-crowding-2020.R outputs.
crowding_1970_obs <- 1.61
crowding_2020_obs <- 1.19
crowding_2020_cf  <- 1.28

# ----- Step 3: Panel builder -----
make_panel <- function(vals, title, ylim, digits = 3) {
  fig_data <- tibble(
    category = factor(
      c("1970\nObserved", "2020\nObserved", "2020\nExpected"),
      levels = c("1970\nObserved", "2020\nObserved", "2020\nExpected")
    ),
    value = vals,
    type  = c("Observed", "Observed", "Expected")
  )

  ggplot(fig_data, aes(x = category, y = value,
                       fill = type, linetype = type)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.3, width = 0.6) +
    geom_text(
      aes(label = sprintf(paste0("%.", digits, "f"), value)),
      vjust = 1.5, color = "white", size = 4
    ) +
    scale_fill_manual(values = c(
      "Observed" = "steelblue",
      "Expected" = scales::alpha("steelblue", 0.5)
    )) +
    scale_linetype_manual(values = c(
      "Observed" = "solid",
      "Expected" = "dotted"
    )) +
    labs(title = title, y = NULL, x = NULL) +
    coord_cartesian(ylim = ylim) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(size = 11, margin = margin(t = 5)),
      plot.title      = element_text(size = 13, hjust = 0.5)
    )
}

# ----- Step 4: Two panels -----
p_bedrooms <- make_panel(
  vals   = c(bedroom_1970_obs, bedroom_2020_obs, bedroom_2020_cf),
  title  = "Bedrooms",
  ylim   = c(2, 3),
  digits = 2
)

p_crowding <- make_panel(
  vals   = c(crowding_1970_obs, crowding_2020_obs, crowding_2020_cf),
  title  = "Crowding (Persons per Bedroom)",
  ylim   = c(1, 2),
  digits = 2
)

fig <- p_bedrooms + p_crowding

# ----- Step 5: Save -----
output_dir <- "bedroom-allocation/output/figures/kob"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  file.path(output_dir, "observed-cf-bedrooms-and-crowding.png"),
  plot  = fig,
  width = 9, height = 4.5, units = "in", dpi = 300
)
