# finish-remaining-chunks.R
#
# Fifth attempt. Previous attempts died at ~16M rows, and the crash is now
# traced to the ipumsr/hipread read path (not arrow, not DuckDB). Bypass it.
#
# Plan:
#   1. Decompress the .dat.gz once (few minutes, one-time).
#   2. readr::read_fwf_chunked(skip = N * 500000) seeks past already-written
#      chunks WITHOUT parsing those rows into R - so we never touch the
#      leak-triggering code path.
#   3. Read only the remaining rows (~1.5M), write to parquet chunks 33+.
#   4. Phase 2 materializes DuckDB from the parquet glob.
#
# Usage:
#   CLOSE R COMPLETELY AND REOPEN first, then source this file.
#   Dependencies: install.packages(c("R.utils", "arrow")) if not present.

library(ipumsr)
library(readr)
library(duckdb)
library(DBI)

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("install.packages('arrow') first")
}
if (!requireNamespace("R.utils", quietly = TRUE)) {
  stop("install.packages('R.utils') first")
}

gz_path    <- "data/raw-microdata/usa_00058.dat.gz"
dat_path   <- "data/raw-microdata/usa_00058.dat"
ddi_path   <- "data/raw-microdata/usa_00058.xml"
db_path    <- "data/bedroom-allocation-kob-db/ipums.duckdb"
chunks_dir <- "data/bedroom-allocation-kob-db/chunks"

if (!dir.exists(chunks_dir)) dir.create(chunks_dir, recursive = TRUE)

# ---- Step 1: Decompress .dat.gz -> .dat ----
if (!file.exists(dat_path)) {
  message("Decompressing .dat.gz -> .dat. This may take several minutes.")
  R.utils::gunzip(gz_path, destname = dat_path, remove = FALSE, overwrite = FALSE)
  message("Done decompressing.")
} else {
  message("Uncompressed .dat already exists, reusing it.")
}

# ---- Step 2: Parse DDI for column spec ----
ddi <- read_ipums_ddi(ddi_path)
vi  <- ddi$var_info

# Print diagnostic — column names of var_info may differ across ipumsr versions
message("DDI var_info columns: ", paste(names(vi), collapse = ", "))

# Find start/end column names (different ipumsr versions use different names)
start_nm <- intersect(c("start", "start_col", "var_start"), names(vi))[1]
end_nm   <- intersect(c("end",   "end_col",   "var_end"),   names(vi))[1]
imp_nm   <- intersect(c("imp_decim", "imp_decimals"),       names(vi))[1]
if (is.na(start_nm) || is.na(end_nm)) {
  stop("Could not find start/end columns in DDI var_info. Columns present: ",
       paste(names(vi), collapse = ", "))
}

# Drop REPWTP* cols
keep_mask <- !grepl("^REPWTP", vi$var_name)
vars_keep <- vi$var_name[keep_mask]
starts    <- vi[[start_nm]][keep_mask]
ends      <- vi[[end_nm]][keep_mask]
imp_dec   <- if (!is.na(imp_nm)) vi[[imp_nm]][keep_mask] else rep(0L, length(vars_keep))

col_positions <- fwf_positions(
  start     = starts,
  end       = ends,
  col_names = vars_keep
)

# ---- Step 3: Determine resume point from existing parquet files ----
existing <- list.files(chunks_dir, pattern = "^chunk_\\d+\\.parquet$")
skip_chunks <- length(existing)
skip_rows   <- skip_chunks * 500000L
message(sprintf("Found %d existing parquet chunks -> skipping %d source rows.",
                skip_chunks, skip_rows))

# ---- Step 4: Read remaining rows (all at once), apply imp_decim, write parquet ----
# Only ~1.5M rows left; fits comfortably in a fresh R session's memory.
# readr::read_fwf with skip seeks past already-processed rows without
# parsing them, so we never hit the leak.

message("Reading remaining rows with readr::read_fwf (this will take a few min)...")
remaining <- read_fwf(
  file          = dat_path,
  col_positions = col_positions,
  col_types     = cols(.default = col_double()),
  skip          = skip_rows,
  progress      = TRUE
)
message(sprintf("Read %d remaining rows.", nrow(remaining)))

# Apply implied decimal places (IPUMS encodes e.g. CPI99 with 4 implied
# decimals). ipumsr does this automatically; readr does not.
imp_decim_factors <- 10 ^ imp_dec
names(imp_decim_factors) <- vars_keep
for (v in vars_keep[imp_dec > 0]) {
  if (v %in% names(remaining)) {
    remaining[[v]] <- remaining[[v]] / imp_decim_factors[[v]]
  }
}

# Split into 500k-row parquet chunks for consistency with existing files
chunk_size   <- 500000L
chunk_counter <- skip_chunks
n <- nrow(remaining)
n_new_chunks <- ceiling(n / chunk_size)
for (i in seq_len(n_new_chunks)) {
  start_row <- (i - 1L) * chunk_size + 1L
  end_row   <- min(i * chunk_size, n)
  chunk     <- remaining[start_row:end_row, ]
  chunk_counter <- chunk_counter + 1L
  out_path  <- file.path(chunks_dir,
                         sprintf("chunk_%04d.parquet", chunk_counter))
  arrow::write_parquet(chunk, out_path)
  message(sprintf("Chunk %d: wrote %d rows -> %s",
                  chunk_counter, nrow(chunk), basename(out_path)))
}
rm(remaining); gc(verbose = FALSE)

message(sprintf("Phase 1 complete. %d total chunks now on disk.",
                chunk_counter))

# ---- Step 5: Materialize DuckDB from all parquet chunks ----
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
message(sprintf("DONE. DuckDB table 'ipums' has %d rows.", n_rows))
dbDisconnect(con, shutdown = TRUE)
