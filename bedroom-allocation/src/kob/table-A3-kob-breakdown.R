# table-A3-kob-breakdown.R
#
# Appendix Table A3: per-level KOB breakdown for endowment (e) and
# coefficient (c) components.
#
# Two Excel sheets in one workbook:
#   - Endowment   (per level, sorted by variable |total e|)
#   - Coefficient (per level, sorted by variable |total c|)
#
# Each begins with a TOTAL row, then per-level detail showing:
#   Variable, Level, Value (e or c), Pct (% of total), coef_1970,
#   coef_2020, prop_1970, prop_2020.
#
# For continuous variables (hoh_age, n_children_under_18, n_adults,
# hhincome_2020_harmonized) the Level column is empty; prop values are
# the variable's weighted mean rather than a proportion.

library(dplyr)
library(tibble)
library(readr)

kob_output <- readRDS("bedroom-allocation/throughput/kob_output.rds")

total_e <- sum(kob_output$e, na.rm = TRUE)
total_c <- sum(kob_output$c, na.rm = TRUE)

# ----- Helper: order variables by absolute total e or c -----
var_order_by <- function(col) {
  kob_output |>
    mutate(variable = if_else(is.na(variable), term, variable)) |>
    filter(variable != "(Intercept)") |>
    group_by(variable) |>
    summarise(total_abs = abs(sum(.data[[col]], na.rm = TRUE)), .groups = "drop") |>
    arrange(desc(total_abs)) |>
    pull(variable)
}

# ----- Helper: build a per-variable summary table for one component -----
# Output:
#   TOTAL row
#   One row per variable (sorted by |variable total|), showing Value and
#   % of total.
build_detail <- function(component_col, total_val, total_label) {
  per_var <- kob_output |>
    filter(term != "(Intercept)") |>
    mutate(Variable = if_else(is.na(variable), term, variable)) |>
    group_by(Variable) |>
    summarise(
      Value = round(sum(.data[[component_col]], na.rm = TRUE), 4),
      .groups = "drop"
    ) |>
    mutate(Pct = round(Value / total_val * 100, 1)) |>
    arrange(desc(abs(Value)))

  bind_rows(
    tibble(
      Variable = total_label,
      Value    = round(total_val, 4),
      Pct      = 100
    ),
    per_var
  )
}

e_detail <- build_detail("e", total_e, "TOTAL (e)")
c_detail <- build_detail("c", total_c, "TOTAL (c)")

options(tibble.width = Inf, tibble.print_max = 200)
cat("=== Endowment (e) breakdown ===\n")
print(e_detail, n = Inf)

cat("\n=== Coefficient (c) breakdown ===\n")
print(c_detail, n = Inf)

# ----- Save -----
output_dir <- "bedroom-allocation/output/tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_csv(e_detail, file.path(output_dir, "table-A3-endowment-breakdown.csv"))
write_csv(c_detail, file.path(output_dir, "table-A3-coefficient-breakdown.csv"))

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    list(Endowment = e_detail, Coefficient = c_detail),
    file.path(output_dir, "table-A3-kob-breakdown.xlsx")
  )
  message("Saved CSVs + multi-sheet XLSX to bedroom-allocation/output/tables/")
} else {
  message("Saved CSVs. Install writexl for xlsx.")
}
