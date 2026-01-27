# import-ipums-usa.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
#
# Input:
# -  makes API call to IPUMS CPS. Be sure to follow Part B of project set-up
#    in README.md before running - this script reads an environment variable from 
#    .Renviron
#

# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("ipumsr")
library("glue")

if (!file.exists(".Renviron")) {
  stop(".Renviron file needed for this code to run. Please refer to Part B of the README file for configuration instructions.")
} 

# Read API key from project-local .Renviron
readRenviron(".Renviron") # Force a re-read each run
api_key <- Sys.getenv("IPUMS_API_KEY")

if (api_key == "" || api_key == "your_ipums_api_key") {
  stop(".Renviron file exists, but IPUMS API key has not been added. Please refer to Part B of the README file for configuration instructions.")
}

print(paste0("IPUMS API key: ", api_key))
set_ipums_api_key(api_key)

# Set the destination directories for the IPUMS data pull
download_dir <- "data/raw-microdata"
db_dir <- "data/five-decade-db"

# ----- Step 1: Define, submit, and wait for data extract ----- #
# Browse available samples and their aliases
get_sample_info("cps") |> print(n=1000) 

# Define extract
ipums_extract <- define_extract_micro(
  description = "Households over the years - CPS pull for interrelationships",
  collection = "cps",
  samples = c(
    "cps1970_03s", # IPUMS-CPS, ASEC 1970 
    "cps1980_03s", # IPUMS-CPS, ASEC 1980 
    "cps1990_03s", # IPUMS-CPS, ASEC 1990 
    "cps2000_03s", # IPUMS-CPS, ASEC 2000  
    "cps2010_03s", # IPUMS-CPS, ASEC 2010
    "cps2020_03s" # IPUMS-CPS, ASEC 2020
  ),
  variables = c(
    # Household-level
    "NUMPREC", "NFAMS", "NCOUPLES", "NMOTHERS", "NFATHERS", "MULTGEN", 
    "ASECWTH", # equivalent to HHWT in ACS
    #"UNITSSTR", #not needed for now
    # Person-level
    "PERNUM", 
    "ASECWT", # equivalent to PERWT in ACS
    "LINENO", "RELATE",
    "FAMSIZE", "FAMUNIT",
    "MOMLOC", "POPLOC", "PELNDAD", "PELNMOM", "PEMOMTYP", 
    "SPLOC",  "ASPOUSE", "PECOHAB",
    "NCHILD",  "NSIBS",
    "SEX", "AGE"
  )
)

# Submit extract request
submitted <- submit_extract(ipums_extract)

# Poll until extract is ready
wait_for_extract(submitted) 

# ----- Step 2: Download and save extract ----- #

# Once ready, download the extract ZIP
download_extract(
  submitted,
  download_dir = download_dir,
  overwrite = TRUE,
  api_key = api_key
)

extract_num <- sprintf("%05d", submitted$number)

ddi_path <- glue("{download_dir}/cps_{extract_num}.xml")
dat_path <- glue("{download_dir}/cps_{extract_num}.dat.gz")

# ----- Step 3: Save to DuckDB ----- #

ddi <- read_ipums_ddi(ddi_path)
ipums_tb <- read_ipums_micro(ddi, var_attrs = c()) 

con <- dbConnect(duckdb::duckdb(), glue("{db_dir}/ipums_cps.duckdb"))
dbWriteTable(con, "ipums", ipums_tb, overwrite = TRUE)
DBI::dbDisconnect(con)