# salvage-extract-to-duckdb.R
#
# Skip-if-exists variant. After four attempts crashed on std::bad_alloc
# (arrow's C++ memory pool accumulating state across many writes), this
# version reuses any chunk_NNNN.parquet files already on disk and only
# writes the ones still missing. Minimal arrow allocation - should finish.
#
# Usage:
#   1. RESTART R COMPLETELY (close window, reopen) so R heap is fresh.
#   2. rm(list = ls()); gc()
#   3. file.remove("data/bedroom-allocation-kob-db/ipums.duckdb")  # if exists
#   4. source(THIS_FILE)
#
# DOES NOT delete existing parquet chunks. To start from scratch instead:
#   unlink("data/bedroom-allocation-kob-db/chunks", recursive = TRUE)

library(ipumsr)
library(duckdb)
library(DBI)
library(dplyr)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("The 'arrow' package is required. Install with: install.packages('arrow')")
}

ddi_path   <- "data/raw-microdata/usa_00058.xml"
db_path    <- "data/bedroom-allocation-kob-db/ipums.duckdb"
chunks_dir <- "data/bedroom-allocation-kob-db/chunks"

if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
if (!dir.exists(chunks_dir))       dir.create(chunks_dir,       recursive = TRUE)

existing <- list.files(chunks_dir, pattern = "^chunk_\\d+\\.parquet$")
message(sprintf("Found %d existing parquet chunks, will skip those.", length(existing)))

ddi       <- read_ipums_ddi(ddi_path)
all_vars  <- ddi$var_info$var_name
keep_vars <- all_vars[!grepl("^REPWTP", all_vars)]

# ---- Phase 1: stream chunks to parquet, skipping those already on disk ----

chunks_done <- 0L
chunks_written <- 0L
callback <- IpumsSideEffectCallback$new(function(chunk, pos) {
  chunks_done <<- chunks_done + 1L
  out_path <- file.path(
    chunks_dir,
    sprintf("chunk_%04d.parquet", chunks_done)
  )
  if (file.exists(out_path)) {
    message(sprintf("Chunk %d: row %d - skipped (exists)", chunks_done, pos))
  } else {
    arrow::write_parquet(chunk, out_path)
    chunks_written <<- chunks_written + 1L
    message(sprintf(
      "Chunk %d: row %d (%d rows, %d cols) -> %s",
      chunks_done, pos, nrow(chunk), ncol(chunk), basename(out_path)
    ))
  }
  gc(verbose = FALSE)
})

read_ipums_micro_chunked(
  ddi,
  callback,
  chunk_size = 500000,
  vars       = all_of(keep_vars),
  var_attrs  = c()
)

message(sprintf("Phase 1 done. %d chunks total, %d newly written.",
                chunks_done, chunks_written))

# ---- Phase 2: materialize DuckDB table from parquet glob ----

con <- dbConnect(duckdb::duckdb(), db_path)
dbExecute(con, "SET memory_limit='4GB'")
temp_dir <- "data/bedroom-allocation-kob-db/duckdb_tmp"
if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
dbExecute(con, sprintf("SET temp_directory='%s'", temp_dir))

if ("ipums" %in% dbListTables(con)) dbRemoveTable(con, "ipums")

chunks_glob_sql <- gsub("\\\\", "/", file.path(chunks_dir, "chunk_*.parquet"))

message("Phase 2: building DuckDB table from parquet glob...")
dbExecute(con, sprintf(
  "CREATE TABLE ipums AS SELECT * FROM read_parquet('%s')",
  chunks_glob_sql
))

dbExecute(con, "CHECKPOINT")

n_rows <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM ipums")$n
message(sprintf("Done. DuckDB table `ipums` has %d rows.", n_rows))

dbDisconnect(con, shutdown = TRUE)
