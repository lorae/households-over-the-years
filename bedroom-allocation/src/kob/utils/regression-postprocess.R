# regression-postprocess.R
#
# Utilities for transforming raw regression output into kob_input format.
# Ported from household-size-demographics/src/utils/regression-postprocess-tools.R
# with these changes: the hardcoded `varnames_dict` is replaced by deriving
# varnames from `names(adjust_by)`; and the `add_intercept[_v2]()` helpers
# are omitted (our kob() engine handles the intercept natively).
#
# Pipeline:
#   split_term_column()       - parses "hoh_race_ethBlack" -> variable + value
#   complete_implicit_zeros() - adds omitted reference levels as 0-coef rows
#   gu_adjust()               - Gardeazabal-Ugidos normalization of coefficients
#   standardize_coefs()       - orchestrator running all three in sequence
#
# Known gap (Phase 7 polish): gu_adjust() transforms coefficients but does
# NOT transform SEs. G-U is a linear combination so SEs should get the same
# transformation via the vcov matrix. Flagged for later.

library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(glue)

# ----- split_term_column ----- #
# Parse regression term like "hoh_race_ethBlack" into variable = "hoh_race_eth"
# and value = "Black". Terms that don't prefix-match any varname get
# variable = NA (typically continuous regressors).
split_term_column <- function(data, varnames) {
  extract_variable <- function(term, varnames) {
    if (term == "(Intercept)") return("(Intercept)")
    matched <- varnames[str_detect(term, fixed(varnames))]
    if (length(matched) > 0) return(matched[1])
    return(NA_character_)
  }

  data <- data |>
    mutate(
      variable = map_chr(term, ~ extract_variable(.x, varnames)),
      value = case_when(
        term == "(Intercept)" ~ "(Intercept)",
        !is.na(variable) ~ str_remove(term, fixed(variable)),
        TRUE ~ term
      )
    ) |>
    select(term, variable, value, everything())

  unmatched <- data |> filter(is.na(variable)) |> pull(term)
  if (length(unmatched) > 0) {
    warning("Unmatched term prefixes (likely continuous vars): ",
            paste(unique(unmatched), collapse = ", "))
  }
  data
}

# ----- complete_implicit_zeros ----- #
# For each variable in `adjust_by`, finds the omitted reference level and adds
# a row with coef = 0 so all K levels are represented explicitly.
complete_implicit_zeros <- function(
    reg_output,
    adjust_by,
    coef_col = "estimate",
    se_col   = NULL
) {
  if (!all(c("variable", "value") %in% names(reg_output))) {
    reg_output <- split_term_column(reg_output, varnames = names(adjust_by))
  }

  present_vars <- intersect(names(adjust_by), unique(reg_output$variable))

  missing_rows <- map_dfr(present_vars, function(var) {
    observed_rows   <- reg_output |> filter(variable == var)
    observed_values <- observed_rows |> pull(value)
    expected_values <- adjust_by[[var]]

    extra_values <- setdiff(observed_values, expected_values)
    if (length(extra_values) > 0) {
      stop(glue(
        "Variable '{var}' has unexpected levels not in adjust_by: ",
        "{paste(extra_values, collapse=', ')}"
      ))
    }

    missing_value <- setdiff(expected_values, observed_values)
    if (length(missing_value) == 0) {
      warning(glue("No omitted level for '{var}' - nothing added."))
      return(tibble())
    }
    if (length(missing_value) > 1) {
      stop(glue(
        "Variable '{var}' has {length(missing_value)} missing levels; expected exactly 1: ",
        "{paste(missing_value, collapse=', ')}"
      ))
    }

    new_row <- tibble(
      term     = paste0(var, missing_value),
      variable = var,
      value    = as.character(missing_value),
      !!coef_col := 0
    )
    if (!is.null(se_col) && se_col %in% names(reg_output)) {
      # Approximate SE for the zero-coef row. Phase 7 gap.
      new_row[[se_col]] <- sqrt(sum(observed_rows[[se_col]]^2, na.rm = TRUE))
    }
    new_row
  })

  bind_rows(reg_output, missing_rows)
}

# ----- gu_adjust ----- #
# Gardeazabal-Ugidos (2004) coefficient transformation. For each variable,
# compute alpha = mean(coef) across all K levels, subtract alpha from each
# coef, and add sum(alphas) to the intercept. Result: coefs sum to 0 within
# each variable; intercept absorbs the level shift. Makes the decomposition
# invariant to which category was originally omitted as reference.
#
# References:
#   https://ideas.repec.org/a/tpr/restat/v86y2004i4p1034-1036.html
#   https://cran.r-project.org/web/packages/oaxaca/vignettes/oaxaca.pdf
gu_adjust <- function(
    reg_output,
    adjust_by,
    coef_col = "estimate"
) {
  if (!all(c("variable", "value") %in% names(reg_output))) {
    reg_output <- split_term_column(reg_output, varnames = names(adjust_by))
  }

  present_vars <- intersect(names(adjust_by), unique(reg_output$variable))
  if (length(present_vars) == 0) {
    stop("None of the adjust_by variables found in regression output.")
  }

  alpha_df <- reg_output |>
    filter(variable %in% present_vars) |>
    group_by(variable) |>
    summarise(alpha = sum(.data[[coef_col]], na.rm = TRUE) / n(), .groups = "drop")

  total_alpha <- sum(alpha_df$alpha, na.rm = TRUE)

  reg_output |>
    left_join(alpha_df, by = "variable") |>
    mutate(
      !!coef_col := case_when(
        term == "(Intercept)"      ~ .data[[coef_col]] + total_alpha,
        variable %in% present_vars ~ .data[[coef_col]] - alpha,
        TRUE                       ~ .data[[coef_col]]
      )
    ) |>
    select(-alpha)
}

# ----- standardize_coefs (orchestrator) ----- #
# Full pipeline: split term column -> complete implicit zeros -> G-U adjust.
standardize_coefs <- function(
    reg_data,
    adjust_by,
    coef_col = "estimate",
    se_col   = "std.error"
) {
  reg_data |>
    split_term_column(varnames = names(adjust_by)) |>
    complete_implicit_zeros(
      adjust_by = adjust_by,
      coef_col  = coef_col,
      se_col    = se_col
    ) |>
    gu_adjust(
      adjust_by = adjust_by,
      coef_col  = coef_col
    ) |>
    arrange(variable, value)
}
