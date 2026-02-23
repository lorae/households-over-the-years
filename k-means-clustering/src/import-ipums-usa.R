# import-ipums-usa.R
#
# This script processes raw IPUMS data and saves it in a DuckDB file.
#
# Input:
# -  makes API call to IPUMS USA. Be sure to follow Part B of project set-up
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
db_dir <- "data/db"

# ----- Step 1: Define, submit, and wait for data extract ----- #
# Browse available samples and their aliases
get_sample_info("usa") |> print(n=200) 

# Define extract
ipums_extract <- define_extract_micro(
  description = "Households over the years",
  collection = "usa",
  samples = c(
    # For more info see https://usa.ipums.org/usa/sampdesc.shtml
    "us1900k", # 1900 1%
    "us1910k", # 1910 1% 
    "us1920a", # 1920 1%
    "us1930a", # 1930 1%
    "us1940a", # 1940 1%
    "us1950a", # 1950 1%
    "us1960a", # 1960 1%
    "us1970c", # 1970 Form 1 Metro
    "us1980b", # 1980 1%
    "us1990b", # 1990 1%
    "us2000g", # 2000 1% 
    "us2006a", # 2006 ACS
    "us2011a", # 2011 ACS
    "us2016a", # 2016 ACS
    "us2021a", # 2021 ACS
    "us2023a" # 2023 ACS (1-year)
  ),
  variables = c(
    # Household-level
    "NUMPREC", "OWNERSHP", "KITCHEN", "ROOMS", "UNITSSTR", "BEDROOMS", "NFAMS",
    "HHINCOME", "RENT", "OWNCOST",
    # Person-level
    "PERNUM", "PERWT", "RELATE", "SEX", "AGE", "RACE", "HISPAN", 
    "SUBFAM", "EMPSTAT", "INCTOT"
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
