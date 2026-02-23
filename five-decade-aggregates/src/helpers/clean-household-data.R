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
    suffix = "_PERNUM"
) {
  
  # Step 1: Create all composite keys on original table
  out <- df
  for (col in translate_cols) {
    key_name <- paste0("hhid_", col)
    out <- out |>
      mutate(!!key_name := paste(hhid, .data[[col]], sep = "-"))
  }
  
  # Step 2: Build lookup table mapping (hhid, source_ref) -> target_ref
  lookup_table <- df |>
    select(hhid, all_of(source_ref_col), all_of(target_ref_col)) |>
    mutate(
      hhid_LINENO = paste(hhid, .data[[source_ref_col]], sep = "-")
    ) |>
    select(hhid_LINENO, all_of(target_ref_col))
  
  # Step 3-4: Join, rename, and clean up for each translate column
  for (col in translate_cols) {
    key_name <- paste0("hhid_", col)
    new_col_name <- paste0(col, suffix)
    
    # Join
    out <- out |>
      left_join(
        lookup_table,
        by = setNames("hhid_LINENO", key_name)
      )
    
    # Rename the joined target_ref column
    # After join, we'll have target_ref_col.x (original) and target_ref_col.y (joined)
    pernum_y <- paste0(target_ref_col, ".y")
    pernum_x <- paste0(target_ref_col, ".x")
    
    out <- out |>
      rename(
        !!target_ref_col := !!pernum_x,
        !!new_col_name := !!pernum_y
      ) |>
      mutate(
        # Preserve 0 as "no reference"
        !!new_col_name := if_else(.data[[col]] == 0, 0, .data[[new_col_name]])
      ) |>
      select(-all_of(key_name))
  }
  
  out
}