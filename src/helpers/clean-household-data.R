#' Translate person-reference variables from one ID system to another
#'
#' This function converts person-reference variables (e.g. CPS `PELNMOM`,
#' which refers to a person's mother via `LINENO`) into a target identifier
#' system (e.g. `PERNUM`) within households.
#'
#' The function works by:
#' 1. Constructing a household-scoped reference key for the source column
#'    (e.g. `hhid + PELNMOM`, where `PELNMOM` stores a `LINENO` value)
#' 2. Constructing a lookup table mapping `(hhid, LINENO)` to `PERNUM`
#' 3. Joining these keys to recover the referenced person's `PERNUM`
#'
#' Missing references coded as `0` are preserved as `0` in the output.
#'
#' @param df A tibble with one row per person
#' @param hhid_col Household identifier column (currently unused; assumed `hhid`)
#' @param target_ref_col Name of target identifier (e.g. `"PERNUM"`)
#' @param source_ref_col Name of source identifier (e.g. `"LINENO"`)
#' @param translate_cols Character vector of columns to translate
#'   (currently implemented for `"PELNMOM"`)
#' @param suffix Suffix appended to translated column names
#'
#' @return A tibble with additional translated reference columns
#' @export
convert_person_refs <- function(
    df,
    hhid_col,
    target_ref_col,    # e.g., "PERNUM"
    source_ref_col,    # e.g., "LINENO"
    translate_cols,    # e.g., c("PELNMOM", "PELNDAD")
    suffix = "_PERNUM" # controls output column naming
) {
  
  # ------------------------------------------------------------------
  # Step 1: Create a household-scoped key for the reference variable.
  # In CPS, PELNMOM stores the mother's LINENO within household.
  # We combine hhid + PELNMOM to uniquely identify the referenced person.
  # ------------------------------------------------------------------
  orig_table <- df |>
    mutate(
      hhid_PELNMOM = paste(hhid, PELNMOM, sep = "-")
    )
  
  # ------------------------------------------------------------------
  # Step 2: Build a lookup table mapping (hhid, LINENO) -> PERNUM.
  # LINENO is unique within household, so this mapping is one-to-one.
  # ------------------------------------------------------------------
  lookup_table <- orig_table |>
    select(hhid, LINENO, PERNUM) |>
    mutate(
      hhid_LINENO = paste(hhid, LINENO, sep = "-")
    ) |>
    select(hhid_LINENO, PERNUM)
  
  # ------------------------------------------------------------------
  # Step 3: Join the original table to the lookup using the composite keys.
  # This recovers the mother's PERNUM for each person.
  # ------------------------------------------------------------------
  out <- orig_table |>
    left_join(
      lookup_table,
      by = c("hhid_PELNMOM" = "hhid_LINENO")
    ) |>
    rename(
      PERNUM = PERNUM.x,
      PELNMOM_PERNUM = PERNUM.y
      ) |>
    mutate(
      # Preserve CPS convention: 0 indicates no mother present
      PELNMOM_PERNUM = if_else(PELNMOM == 0, 0, PELNMOM_PERNUM)
    ) |>
    select(-hhid_PELNMOM)
  
  out
}

