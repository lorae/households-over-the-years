# Shared helpers and constants for five-decade-aggregates scripts

clean_years <- function(df) {
  df |>
    mutate(
      YEAR = dplyr::recode(YEAR, `2012` = 2010L, `2022` = 2020L)
    )
}

years <- c(1970, 1980, 1990, 2000, 2010, 2020)

race_levels <- c(
  "AIAN", "AAPI", "Black", "Hispanic",
  "White", "Multiracial", "Other"
)

tenure_levels <- c("owner", "renter")

birthplace_levels <- c("U.S.-born", "foreign-born")

income_levels <- c(
  "less than $50,000",
  "$50,000 - $99,999",
  "$100,000 - $149,999",
  "$150,000 and greater"
)
