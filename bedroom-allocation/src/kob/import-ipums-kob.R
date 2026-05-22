# import-ipums-kob.R
#
# Downloads a KOB-specific IPUMS USA extract covering 1970 and 2020 only.
# Narrower than the five-decade-aggregates extract: fewer samples, but adds
# education (EDUC, EDUCD) and household replicate weights (REPWT) needed for
# Successive Differences Replication variance estimation on the 2020 ACS.
# Also pulls CPI99 for inflation adjustment.
#
# Person replicate weights (REPWTP) are intentionally NOT pulled — they're
# not needed for HH-level KOB and ~80 extra columns per row triggers a
# memory leak in ipumsr's chunked read path at ~16M rows.
#
# Read strategy (learned the hard way from extract 58):
# Instead of ipumsr's `read_ipums_micro[_chunked]`, which leaks memory when
# streaming the full gzipped extract, this:
#   1. Decompresses .dat.gz -> .dat once (few min, ~5-10 GB on disk).
#   2. Streams the .dat file via readr::read_lines_chunked (line-based,
#      no accumulated state) and parses each line chunk as fixed-width in
#      an isolated readr::read_fwf call.
#   3. Writes each parsed chunk to its own parquet file.
#   4. Materializes the final DuckDB table from read_parquet('*.parquet').
#
# Inputs:
# - .Renviron (IPUMS_API_KEY)
# - IPUMS USA API (remote)
#
# Outputs:
# - data/raw-microdata/usa_NNNNN.xml + .dat.gz + .dat (intermediates)
# - data/bedroom-allocation-kob-db/chunks/chunk_NNNN.parquet (intermediates)
# - data/bedroom-allocation-kob-db/ipums.duckdb (table: ipums)
#
# The .dat and parquet chunks are kept as on-disk backups — safe to delete
# manually once the DuckDB table is verified good.
#

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("DBI")
library("ipumsr")
library("readr")
library("glue")

if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("install.packages('arrow') first.")
}
if (!requireNamespace("R.utils", quietly = TRUE)) {
  stop("install.packages('R.utils') first.")
}

if (!file.exists(".Renviron")) {
  stop(".Renviron file needed for this code to run. Please refer to Part B of the README file for configuration instructions.")
}

# Read API key from project-local .Renviron
readRenviron(".Renviron") # Force a re-read each run
api_key <- Sys.getenv("IPUMS_API_KEY")

if (api_key == "" || api_key == "your_ipums_api_key") {
  stop(".Renviron file exists, but IPUMS API key has not been added. Please refer to Part B of the README file for configuration instructions.")
}

set_ipums_api_key(api_key)

# Paths
download_dir <- "data/raw-microdata"
db_dir       <- "data/bedroom-allocation-kob-db"
chunks_dir   <- file.path(db_dir, "chunks")

if (!dir.exists(db_dir))     dir.create(db_dir,     recursive = TRUE)
if (!dir.exists(chunks_dir)) dir.create(chunks_dir, recursive = TRUE)

# ----- Step 1: Define, submit, and wait for data extract ----- #
# Browse available samples and their aliases
get_sample_info("usa") |> print(n = 200)

ipums_extract <- define_extract_micro(
  description = "Bedroom allocation KOB: 1970 + 2020 with EDUC and REPWT",
  collection  = "usa",
  samples = c(
    # For more info see https://usa.ipums.org/usa/sampdesc.shtml
    "us1970c", # 1970 Form 1 Metro 1%
    "us2022c"  # 2018-2022 ACS 5-year
  ),
  variables = c(
    # Household-level
    "NUMPREC", "OWNERSHP", "BEDROOMS", "ROOMS", "HHINCOME", "STATEFIP",
    "REGION", "CLUSTER", "STRATA", "CPI99",
    # HH replicate weights for SDR variance (2020 only; auto-skipped for 1970)
    "REPWT",
    # Person-level
    "PERNUM", "PERWT", "RELATE", "SEX", "AGE", "RACE", "HISPAN", "BPL",
    "EDUC", "EDUCD", "INCTOT"
  )
)

submitted <- submit_extract(ipums_extract)
wait_for_extract(submitted)

# ----- Step 2: Download extract ----- #
download_extract(
  submitted,
  download_dir = download_dir,
  overwrite    = TRUE,
  api_key      = api_key
)

extract_num <- sprintf("%05d", submitted$number)
ddi_path    <- glue("{download_dir}/usa_{extract_num}.xml")
gz_path     <- glue("{download_dir}/usa_{extract_num}.dat.gz")
dat_path    <- glue("{download_dir}/usa_{extract_num}.dat")

# ----- Step 3: Decompress .dat.gz -> .dat ----- #
# ipumsr/hipread leaks memory when streaming the gzipped file (~2 GB read
# threshold). Decompressing once and reading via readr bypasses the leak.
if (!file.exists(dat_path)) {
  message("Decompressing .dat.gz -> .dat (may take several minutes)...")
  R.utils::gunzip(gz_path, destname = dat_path, remove = FALSE, overwrite = FALSE)
  message("Decompression done.")
} else {
  message("Uncompressed .dat already exists, reusing it.")
}

# ----- Step 4: Parse DDI for readr column spec ----- #
ddi <- read_ipums_ddi(ddi_path)
vi  <- ddi$var_info

keep_mask <- !grepl("^REPWTP", vi$var_name)
vars_keep <- vi$var_name[keep_mask]
starts    <- vi$start[keep_mask]
ends      <- vi$end[keep_mask]
imp_dec   <- vi$imp_decim[keep_mask]

col_positions <- fwf_positions(
  start     = starts,
  end       = ends,
  col_names = vars_keep
)

imp_decim_factors <- 10 ^ imp_dec
names(imp_decim_factors) <- vars_keep

# ----- Step 5: Chunked read -> parquet ----- #
# Clear any stale chunk files from a prior partial run
stale <- list.files(chunks_dir, pattern = "^chunk_\\d+\\.parquet$", full.names = TRUE)
if (length(stale) > 0) {
  message(sprintf("Removing %d stale parquet chunks from prior run.", length(stale)))
  unlink(stale)
}

chunk_counter <- 0L

chunk_cb <- function(lines_chunk, pos) {
  chunk_counter <<- chunk_counter + 1L
  # Parse this chunk's lines as fixed-width. Writing to a temp file is
  # simpler + more robust than relying on I(...) semantics.
  tmp <- tempfile(fileext = ".dat")
  writeLines(lines_chunk, tmp)
  df <- read_fwf(
    tmp,
    col_positions = col_positions,
    col_types     = cols(.default = col_double()),
    progress      = FALSE
  )
  unlink(tmp)
  # IPUMS encodes some vars (e.g. CPI99) with implied decimals. ipumsr does
  # this automatically on its read path; readr does not.
  for (v in vars_keep[imp_dec > 0]) {
    if (v %in% names(df)) df[[v]] <- df[[v]] / imp_decim_factors[[v]]
  }
  out_path <- file.path(
    chunks_dir,
    sprintf("chunk_%04d.parquet", chunk_counter)
  )
  arrow::write_parquet(df, out_path)
  message(sprintf("Chunk %d: wrote %d rows -> %s",
                  chunk_counter, nrow(df), basename(out_path)))
  rm(df); gc(verbose = FALSE)
}

read_lines_chunked(
  dat_path,
  callback   = SideEffectChunkCallback$new(chunk_cb),
  chunk_size = 500000,
  progress   = TRUE
)

message(sprintf("Phase 1 done. %d parquet chunks written.", chunk_counter))

# ----- Step 6: Materialize DuckDB table from parquet glob ----- #
db_path <- glue("{db_dir}/ipums.duckdb")
con     <- dbConnect(duckdb::duckdb(), db_path)
dbExecute(con, "SET memory_limit='4GB'")
temp_dir <- file.path(db_dir, "duckdb_tmp")
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
message(sprintf("Done. DuckDB table 'ipums' has %d rows.", n_rows))

dbDisconnect(con, shutdown = TRUE)
