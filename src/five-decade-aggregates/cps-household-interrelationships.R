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

# ===== TESTING MODE: Sample households, get all their persons =====
set.seed(123)
n_households = 1000

# Get a sample of household IDs
sampled_hhids <- ipums_person |>
  distinct(hhid) |>
  collect() |>
  slice_sample(n = n_households) |>
  pull(hhid)

# Get ALL persons from those households
sample_person_data <- ipums_person |>
  filter(hhid %in% local(sampled_hhids)) |>
  collect()

# Get unique households from the sample
all_households <- sample_person_data |>
  distinct(hhid) |>
  pull(hhid)
# ==========================================

cat("Total households:", length(all_households), "\n")

# Define batch size
batch_size <- 100

# Split into batches
n_batches <- ceiling(length(all_households) / batch_size)
cat("Processing in", n_batches, "batches\n")

# Drop table if it exists (allows for overwrite on re-runs)
dbExecute(con, "DROP TABLE IF EXISTS ipums_person_with_subfamilies")

# Process each batch
for (i in 1:n_batches) {
  cat("Processing batch", i, "of", n_batches, "...\n")
  
  # Get household IDs for this batch
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, length(all_households))
  batch_hhids <- all_households[start_idx:end_idx]
  
  # Get data for this batch (from sample instead of database)
  batch_data <- sample_person_data |>
    filter(hhid %in% batch_hhids)
  
  # Process households in this batch
  batch_results <- batch_data |>
    group_by(hhid) |>
    group_split() |>
    map_dfr(function(hh) {
      hh <- hh |>
        mutate(id = row_number())
      
      hh_prep <- hh |>
        mutate(
          mother_id = MOMLOC,
          father_id = POPLOC,
          spouse_id = SPLOC,
          age = AGE
        )
      
      # Generate adjacency matrices and derived counts
      adj_mat <- household_adjacency(hh_prep)
      comps <- count_components(adj_mat)
      n_children <- count_children(hh_prep)
      
      hh |>
        mutate(
          # Subfamily structure variables
          n_subfamilies = comps$no,
          subfamily_id = comps$membership,
          subfamily_size = comps$csize[comps$membership],
          nonsubfamily_size = NUMPREC - subfamily_size,
          # Relationship counts
          n_children = n_children,
          # Spouse indicator: 1 if SPLOC references a spouse, 0 otherwise
          n_spouse = if_else(!is.na(SPLOC) & SPLOC > 0, 1, 0)
        )
    })
  
  # Write to DuckDB (append after first batch)
  dbWriteTable(con, "ipums_person_with_subfamilies", batch_results, 
               append = (i > 1))
  
  cat("  Wrote", nrow(batch_results), "rows to database\n")
}

cat("Complete! Table ipums_person_with_subfamilies created\n")

# Force DuckDB to write all data to disk
cat("Flushing data to disk...\n")
dbExecute(con, "CHECKPOINT")

# Verify the table was saved properly
cat("Verifying saved table...\n")
verification <- tbl(con, "ipums_person_with_subfamilies") |> 
  summarise(total_rows = n()) |>
  collect()

cat("SUCCESS: Table saved to DuckDB with", verification$total_rows, "rows\n")

# Check some sample output
tbl(con, "ipums_person_with_subfamilies") |>
  select(hhid, perid, AGE, SEX, NUMPREC, PERNUM, MOMLOC, POPLOC, SPLOC, 
         n_subfamilies, subfamily_id, subfamily_size, nonsubfamily_size, 
         n_children, n_spouse) |>
  head(20) |>
  collect() |>
  print(width = Inf)

# Close connection to ensure data is persisted
dbDisconnect(con, shutdown = TRUE)
cat("Database connection closed. Data is safely persisted.\n")