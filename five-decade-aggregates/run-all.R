# Source this file to replicate the five-decade aggregates analysis.
# BEFORE RUNNING: Create the .Renviron per the instructions in README.md.

# R environment setup
renv::restore()

# Import data
source("five-decade-aggregates/src/import-ipums-usa-1970-2020.R")
source("five-decade-aggregates/src/import-ipums-cps-1970-2020.R")

# Process data
source("five-decade-aggregates/src/process-ipums-usa-person-1970-2020.R")
source("five-decade-aggregates/src/process-ipums-cps-person-1970-2020.R")

# CPS household interrelationships (must run before cps-interrelationships)
source("five-decade-aggregates/src/cps-household-interrelationships.R")
# source("five-decade-aggregates/src/cps-interrelationships.R")

# Generate tables (order matters: some scripts read CSVs from earlier ones)
source("five-decade-aggregates/src/tables/generate-household-bedroom-crowding-overall.R")
source("five-decade-aggregates/src/tables/generate-crowding-overall-subgroup.R")
source("five-decade-aggregates/src/generate-crowding-state-decade.R")
source("five-decade-aggregates/src/generate-crowding-changes-state-1970-2020.R")
source("five-decade-aggregates/src/generate-crowding-age-decade.R")
source("five-decade-aggregates/src/generate-crowding-changes-race-age.R")
source("five-decade-aggregates/src/generate-hhsize-subgroup.R")
source("five-decade-aggregates/src/generate-hhsize-buckets-race-decade.R")
source("five-decade-aggregates/src/generate-hhsize-bedroom-contrib-decade.R")
source("five-decade-aggregates/src/generate-hhsize-contributions-members.R")
source("five-decade-aggregates/src/generate-doubled-up-overall-subgroup.R")
source("five-decade-aggregates/src/generate-relative-crowding-risk.R")
source("five-decade-aggregates/src/compare-surveys.R")

# Generate figures and formatted tables
# (source individual scripts from five-decade-aggregates/src/figures/ as needed)
