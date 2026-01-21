
convert_person_refs <- function(
  df,
  hhid_col,
  target_ref_col,    # e.g., "PERNUM"
  source_ref_col,    # e.g., "LINENO"
  translate_cols,    # e.g., c("PELNMOM", "PELNDAD")
  suffix = "_PERNUM" # controls output column naming
) {
  orig_table <- df |>
    mutate(
      hhid_PELNMOM = paste(hhid, PELNMOM, sep = "-"), # create row uniquely IDing mom
    )
  
  lookup_table <- orig_table |>
    select(hhid, LINENO, PERNUM) |>
    mutate(
      hhid_LINENO = paste(hhid, LINENO, sep = "-") # create row uniquely IDing mom
    ) |>
    select(hhid_LINENO, PERNUM)
  
  print(orig_table)
  print(lookup_table)
  
  out <- orig_table |>
    left_join(
      lookup_table,
      by = c("hhid_PELNMOM" = "hhid_LINENO")
    ) |>
    rename(PELNMOM_PERNUM = PERNUM.y) |>
    mutate(
      PELNMOM_PERNUM = if_else(PELNMOM == 0, 0, PELNMOM_PERNUM)
    ) |>
    select(-PERNUM.y)
  
  out
  
}


# LINENO → PERNUM lookup within household
id_map <- cps_db |>
  mutate(hhid = paste(YEAR, MONTH, SERIAL, sep = "-")) |>
  select(hhid, LINENO, PERNUM_map = PERNUM)

# Check what the MOMLOC values are
cps_db |>
  select(MOMLOC) |>
  collect() |>
  pull(MOMLOC) |>
  unique()


# ASPOUSE, PECOHAB, PELNDAD, and PELNMOM refer to line number, rather than person number
# Using above lookup table we translate this to person number
clean_db <- cps_db |>
  mutate(hhid = paste(YEAR, MONTH, SERIAL, sep = "-")) |>
  
  left_join(id_map, by = c("hhid", "ASPOUSE" = "LINENO")) |>
  rename(ASPOUSE_PERNUM = PERNUM_map) |>
  
  left_join(id_map, by = c("hhid", "PECOHAB" = "LINENO")) |>
  rename(PECOHAB_PERNUM = PERNUM_map) |>
  
  left_join(id_map, by = c("hhid", "PELNDAD" = "LINENO")) |>
  rename(PELNDAD_PERNUM = PERNUM_map) |>
  
  left_join(id_map, by = c("hhid", "PELNMOM" = "LINENO")) |>
  rename(PELNMOM_PERNUM = PERNUM_map)

validate_row_counts(clean_db, obs_count, "Re-mapped ASPOUSE, PECOHAB, PELNMOM, PELNDAD")