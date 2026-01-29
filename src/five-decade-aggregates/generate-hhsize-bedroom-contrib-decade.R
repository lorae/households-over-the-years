# generate-hhsize-bedroom-contrib-decade.R
library(readr)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "output/five-decade-tables/raw"
out_dir  <- "output/five-decade-tables/raw"

hhsize   <- read_csv(file.path(data_dir, "hhsize_decade_overall.csv"), show_col_types = FALSE)
bedrooms <- read_csv(file.path(data_dir, "bedroom_decade_overall.csv"), show_col_types = FALSE)
  
hhsize_bedrooms <- hhsize |>
  left_join(
    bedrooms |> select(YEAR, bedroom),
    by = "YEAR"
  ) |>
  arrange(YEAR) |>
  mutate(
    decade_label = if_else(
      is.na(lag(YEAR)),
      NA_character_,
      paste0(lag(YEAR), "-", YEAR)
    ),
    change_hhsize = hhsize - lag(hhsize),
    change_bedroom = bedroom - lag(bedroom)
  ) |>
  relocate(decade_label, .after = YEAR)|>
  mutate(
    contrib_percent_hhsize = -change_hhsize / (change_bedroom - change_hhsize),
    contrib_percent_bedroom = change_bedroom / (change_bedroom - change_hhsize)
  )

write_csv(
  hhsize_bedrooms,
  file.path(out_dir, "intra_decade_crowding_contributions.csv")
)