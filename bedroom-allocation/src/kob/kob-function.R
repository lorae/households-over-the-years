# kob-function.R
#
# Kitagawa-Oaxaca-Blinder decomposition engine. Ported + generalized from
# household-size-demographics/kob/scripts/kob-function.R. The
# base_year / compare_year parameterization lets callers apply
# base-year coefficients to compare-year population (bedroom-allocation
# convention: base_year = 1970, compare_year = 2020), yielding the
# counterfactual beta_base * X_compare.
#
# For standard errors, we assume independent samples across periods (no
# covariance term between base and compare estimates).
# Formulas drawn from the Census 2019 ACS Accuracy worked examples:
# https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/2019_ACS_Accuracy_Document_Worked_Examples.pdf

library(dplyr)
library(purrr)
library(stringr)
library(glue)


kob <- function(kob_input, base_year, compare_year) {

  # --- Step 0: dynamic column names ------------------------------------------
  # Columns are named using the caller's year values, e.g. coef_1970,
  # coef_1970_se, prop_2020, etc.
  coef_base_col    <- paste0("coef_", base_year)
  coef_base_se_col <- paste0("coef_", base_year, "_se")
  coef_cmp_col     <- paste0("coef_", compare_year)
  coef_cmp_se_col  <- paste0("coef_", compare_year, "_se")
  prop_base_col    <- paste0("prop_", base_year)
  prop_base_se_col <- paste0("prop_", base_year, "_se")
  prop_cmp_col     <- paste0("prop_", compare_year)
  prop_cmp_se_col  <- paste0("prop_", compare_year, "_se")

  # --- Step 1: input checks --------------------------------------------------
  required_cols <- c(
    "term",
    coef_base_col, coef_base_se_col,
    coef_cmp_col,  coef_cmp_se_col,
    prop_base_col, prop_base_se_col,
    prop_cmp_col,  prop_cmp_se_col
  )
  missing_cols <- setdiff(required_cols, names(kob_input))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  intercept_count <- sum(kob_input$term == "(Intercept)")
  if (intercept_count > 1) {
    stop("❌ Multiple '(Intercept)' rows detected. Only one is allowed.")
  }

  # --- Step 2: initialize output ------------------------------------------
  kob_output <- kob_input |>
    mutate(
      u = NA_real_, u_se = NA_real_,
      e = NA_real_, e_se = NA_real_,
      c = NA_real_, c_se = NA_real_
    )

  # --- Step 3a: intercept (u) ---------------------------------------------
  # u = coef_compare - coef_base (evaluated only on the intercept row)
  if (intercept_count == 1) {
    message("✅ Intercept detected. u term will be calculated.")

    intercept_row <- kob_input |> filter(term == "(Intercept)")
    b     <- intercept_row[[coef_base_col]]
    c_    <- intercept_row[[coef_cmp_col]]
    b_se  <- intercept_row[[coef_base_se_col]]
    c_se_ <- intercept_row[[coef_cmp_se_col]]

    u_val    <- c_ - b
    u_se_val <- sqrt(b_se^2 + c_se_^2)

    kob_output <- kob_output |>
      mutate(
        u    = case_when(term == "(Intercept)" ~ u_val,    TRUE ~ u),
        u_se = case_when(term == "(Intercept)" ~ u_se_val, TRUE ~ u_se)
      )
  } else {
    message("ℹ️ Intercept not detected. u term will not be calculated.")
  }

  # --- Step 3b: endowment (e) ---------------------------------------------
  # e = coef_base * (prop_compare - prop_base)   (composition shift priced at BASE)
  kob_output <- kob_output |>
    mutate(
      e     = .data[[coef_base_col]] * (.data[[prop_cmp_col]] - .data[[prop_base_col]]),
      .x    = .data[[coef_base_col]],
      .y    = .data[[prop_cmp_col]] - .data[[prop_base_col]],
      .se_x = .data[[coef_base_se_col]],
      .se_y = sqrt(.data[[prop_base_se_col]]^2 + .data[[prop_cmp_se_col]]^2),
      e_se  = sqrt(.x^2 * .se_y^2 + .y^2 * .se_x^2)
    ) |>
    select(-.x, -.y, -.se_x, -.se_y)

  # --- Step 3c: coefficient effect (c) ------------------------------------
  # c = (coef_compare - coef_base) * prop_compare  (coef shift weighted by COMPARE pop)
  kob_output <- kob_output |>
    mutate(
      c     = (.data[[coef_cmp_col]] - .data[[coef_base_col]]) * .data[[prop_cmp_col]],
      .x    = .data[[coef_cmp_col]] - .data[[coef_base_col]],
      .y    = .data[[prop_cmp_col]],
      .se_x = sqrt(.data[[coef_base_se_col]]^2 + .data[[coef_cmp_se_col]]^2),
      .se_y = .data[[prop_cmp_se_col]],
      c_se  = sqrt(.x^2 * .se_y^2 + .y^2 * .se_x^2)
    ) |>
    select(-.x, -.y, -.se_x, -.se_y)

  kob_output
}


# Validation helper: sum of (u + e + c) across all terms should equal
# (mean_compare - mean_base) within tolerance.
kob_output_validate <- function(
    kob_output, mean_base, mean_compare, tol = 1e-10
) {
  actual <- kob_output |>
    mutate(row_sum = rowSums(across(c(u, e, c)), na.rm = TRUE)) |>
    summarise(actual = sum(row_sum)) |>
    pull(actual)

  expected   <- mean_compare - mean_base
  difference <- abs(actual - expected)

  if (difference <= tol) {
    message(glue("✅ Decomposition matches expected total within {tol} tolerance."))
    invisible(TRUE)
  } else {
    stop(glue(
      "❌ Decomposition mismatch:
      actual   = {round(actual, 6)}
      expected = {round(expected, 6)}
      diff     = {round(difference, 6)}"
    ))
  }
}
