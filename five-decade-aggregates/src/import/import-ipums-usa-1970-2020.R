# import-ipums-usa-1970-2020.R
#
# Downloads IPUMS USA microdata (1970-2020) via the IPUMS API and loads it into
# a DuckDB database. Pulls six decennial/ACS samples with household and person
# variables needed for the five-decade aggregates analysis.
#
# Inputs:
# - .Renviron (IPUMS_API_KEY)
# - IPUMS USA API (remote)
#
# Outputs:
# - data/raw-microdata/usa_NNNNN.xml + .dat.gz (intermediate)
# - data/five-decade-db/ipums.duckdb (table: ipums)
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
get_sample_info("usa") |> print(n=200) 

# Define extract
ipums_extract <- define_extract_micro(
  description = "Households over the years",
  collection = "usa",
  samples = c(
    # For more info see https://usa.ipums.org/usa/sampdesc.shtml
    "us1970c", # 1970 Form 1 Metro
    "us1980b", # 1980 1%
    "us1990b", # 1990 1%
    "us2000g", # 2000 1% 
    "us2012e", # 2008-2012, ACS 5-year
    "us2022c" # 2018-2022, ACS 5-year
  ),
  variables = c(
    # Household-level
    "NUMPREC", "OWNERSHP", "KITCHEN", "ROOMS", "UNITSSTR", "BEDROOMS", "NFAMS",
    "HHINCOME", "RENT", "OWNCOST", "STATEFIP",
    # Person-level
    "PERNUM", "PERWT", "RELATE", "SEX", "AGE", "RACE", "HISPAN", 
    "SUBFAM", "EMPSTAT", "INCTOT", "BPL"
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

ddi_path <- glue("{download_dir}/usa_{extract_num}.xml")
dat_path <- glue("{download_dir}/usa_{extract_num}.dat.gz")

# ----- Step 3: Save to DuckDB ----- #

ddi <- read_ipums_ddi(ddi_path)
ipums_tb <- read_ipums_micro(ddi, var_attrs = c()) 

con <- dbConnect(duckdb::duckdb(), glue("{db_dir}/ipums.duckdb"))
dbWriteTable(con, "ipums", ipums_tb, overwrite = TRUE)
DBI::dbDisconnect(con)
