# fig04-single-mother-rent-burden-regressions.R
#
# Regression of rent burden among single-mother households (renters and
# homeowners) on year, bedrooms, and tenure.
#
# Inputs:
# - household-archetypes/throughput/single-mothers.duckdb (table: single_mothers)
#
# Outputs:
# - household-archetypes/output/tables/fig04-single-mother-rent-burden-regressions.csv

library(dplyr)
library(dbplyr)
library(duckdb)
library(modelsummary)

con <- dbConnect(duckdb::duckdb(), "household-archetypes/throughput/single-mothers.duckdb", read_only = TRUE)

single_mothers <- tbl(con, "single_mothers")

# --- prep regression data ---

reg_data <- single_mothers |>
  filter(
    RELATE == 1,
    OWNERSHP %in% c(1, 2),
    HHINCOME > 0
  ) |>
  mutate(
    housing_cost = case_when(
      OWNERSHP == 1L ~ OWNCOST,
      OWNERSHP == 2L ~ RENT
    ),
    cost_burden = (housing_cost * 12) / HHINCOME,
    decades_since_1970 = case_when(
      YEAR == 1970L ~ 0L,
      YEAR == 1980L ~ 1L,
      YEAR == 1990L ~ 2L,
      YEAR == 2000L ~ 3L,
      YEAR == 2012L ~ 4L,
      YEAR == 2022L ~ 5L
    ),
    bedrooms_recode = pmin(BEDROOMS - 1L, 5L),
    age_minus_18 = AGE - 18L,
    RACE_bucket = case_when(
      RACE == 1L ~ "white",
      RACE == 2L ~ "black",
      RACE == 3L ~ "aian",
      RACE %in% c(4L, 5L, 6L) ~ "aapi",
      RACE == 7L ~ "other",
      RACE %in% c(8L, 9L) ~ "multi"
    ),
    HISPAN_bucket = case_when(
      HISPAN == 0L ~ "not_hispanic",
      HISPAN %in% c(1L, 2L, 3L, 4L) ~ "hispanic",
      HISPAN == 9L ~ "N/A"
    )
  ) |>
  filter(cost_burden < 1) |>
  collect() |>
  mutate(
    tenure = factor(OWNERSHP, levels = c(2, 1), labels = c("renter", "homeowner")),
    hispan_binary = as.integer(HISPAN_bucket == "hispanic"),
    race = factor(RACE_bucket, levels = c("white", "aapi", "aian", "black", "multi", "other"))
  )

dbDisconnect(con)

# --- regression 1: cost burden ~ decades since 1970 + bedrooms (categorical) ---

m1 <- lm(cost_burden ~ decades_since_1970 + factor(bedrooms_recode), data = reg_data)

# --- regression 2: add tenure (renter as reference) ---

m2 <- lm(cost_burden ~ decades_since_1970 + factor(bedrooms_recode) + tenure, data = reg_data)

# --- regression 3: add age (minus 18) as a continuous control ---

m3 <- lm(cost_burden ~ decades_since_1970 + factor(bedrooms_recode) + tenure + age_minus_18, data = reg_data)

# --- regression 4: add hispan binary and race/eth (White omitted) ---

m4 <- lm(
  cost_burden ~ decades_since_1970 + factor(bedrooms_recode) + tenure + age_minus_18 +
    hispan_binary + race,
  data = reg_data
)

# --- regression 5: add state fixed effects (coefficients hidden in output) ---

m5 <- lm(
  cost_burden ~ decades_since_1970 + factor(bedrooms_recode) + tenure + age_minus_18 +
    hispan_binary + race + factor(STATEFIP),
  data = reg_data
)

modelsummary(
  list("Model 1" = m1, "Model 2" = m2, "Model 3" = m3, "Model 4" = m4, "Model 5" = m5),
  output = "household-archetypes/output/tables/fig04-single-mother-rent-burden-regressions.docx",
  stars = TRUE,
  coef_omit = "STATEFIP",
  add_rows = tibble::tribble(
    ~term, ~"Model 1", ~"Model 2", ~"Model 3", ~"Model 4", ~"Model 5",
    "State FE", "No", "No", "No", "No", "Yes"
  )
)
