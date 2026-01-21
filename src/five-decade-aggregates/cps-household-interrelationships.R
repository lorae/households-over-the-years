# The purpose of this script is to quantify the frequency of household interrelationships
# across all decades in the sample.

# ----- Step 0: CPS ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")
library("igraph")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/five-decade-db/ipums_cps.duckdb")
ipums_person <- tbl(con, "ipums_person") |>
  mutate(
    # Uniquely ID households and persons
    # https://cps.ipums.org/cps-action/variables/SERIAL#description_section
    hhid = paste(
      as.integer(YEAR), 
      as.integer(MONTH), 
      as.integer(SERIAL), sep = "-"), # households
    perid = paste(
      hhid, 
      as.integer(PERNUM), sep = "-") # persons
  )

# Sample 100 random households
set.seed(123)  # for reproducibility

sampled_households <- ipums_person |>
  distinct(hhid) |>
  collect() |>
  slice_sample(n = 100)

# Get all person records for these 100 households
sample_data <- ipums_person |>
  filter(hhid %in% local(sampled_households$hhid)) |>
  collect()

# Process each household
results <- sample_data |>
  group_by(hhid) |>
  group_split() |>
  map_dfr(function(hh) {
    # Create row-numbered id for adjacency matrix
    hh <- hh |>
      mutate(id = row_number())
    
    # Rename to expected column names
    hh_prep <- hh |>
      mutate(
        mother_id = MOMLOC,
        father_id = POPLOC,
        spouse_id = SPLOC,
        age = AGE
      )
    
    # Build adjacency matrix
    adj_mat <- household_adjacency(hh_prep)
    
    # Count components
    comps <- count_components(adj_mat)
    
    # Add subfamily info to each person
    hh |>
      mutate(
        n_subfamilies = comps$no,
        subfamily_id = comps$membership
      )
  })

# Check the output
results |>
  select(hhid, perid, PERNUM, MOMLOC, POPLOC, SPLOC, n_subfamilies, subfamily_id) |>
  View()
