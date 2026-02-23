# fig02-household-members
# 
# Understand the configuration of a household 
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("purrr")
library("tidyr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_household <- tbl(con, "ipums_household")

# ----- Step 1: Produce data ----- #
# Helper function to compute weighted mean by YEAR and rename column
get_component_mean <- function(var) {
  crosstab_mean(
    data = ipums_household |> filter(GQ %in% c(0,1,2)),
    value = var,
    wt_col = "HHWT",
    group_by = c("YEAR")
  ) |>
    select(YEAR, weighted_mean) |>
    rename(!!var := weighted_mean)
}

# Variables to process
vars <- c(
  "n_spouse",
  "n_child",
  "n_parent",
  "n_grandchild",
  "n_other_rel",
  "n_non_rel"
)

# Apply the function to each variable and reduce with left_join
hh_components <- vars |>
  map(get_component_mean) |>
  reduce(left_join, by = "YEAR")


# Add reference person column
hh_long <- hh_components |>
  mutate(reference_person = 1) |>
  pivot_longer(
    cols = -YEAR,
    names_to = "component",
    values_to = "value"
  )

# Reorder components for nicer stacking
hh_long <- hh_long |>
  mutate(component = factor(
    component,
    levels = c(
      "n_non_rel",
      "n_other_rel",
      "n_parent",
      "n_grandchild",
      "n_child",
      "n_spouse",
      "reference_person"
    ),
    labels = c(
      "Non-relatives",
      "Other relatives",
      "Parents / in-laws",
      "Grandchildren",
      "Children",
      "Spouse",
      "Reference person"
    )
  ))

# ----- Step 2: Create plot ----- #
fig02 <- ggplot(hh_long, aes(x = YEAR, y = value, fill = component)) +
  geom_area(color = "white", size = 0.2, alpha = 0.9) +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Household Composition Over Time",
    x = NULL,
    y = "Average number of people in household",
    fill = "Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

fig02

# ----- Step 3: Save data and plot ----- #

write_csv(
  hh_components,
  "k-means-clustering/output/figure-data/fig02-household-members-sand.csv"
)

ggsave(
  filename = "k-means-clustering/output/figures/fig02-household-members-sand.jpeg",
  plot = fig02,
  width = 6,
  height = 4,
  dpi = 500,
  scale = 1.5
)


