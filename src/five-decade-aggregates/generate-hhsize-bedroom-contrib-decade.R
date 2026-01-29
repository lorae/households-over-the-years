# generate-hhsize-bedroom-contrib-decade.R
library(readr)

# ----------------------------
# Paths
# ----------------------------
data_dir <- "output/five-decade-tables/raw"
out_dir  <- "output/five-decade-tables"

hhsize   <- read_csv(file.path(data_dir, "hhsize_decade_overall.csv"), show_col_types = FALSE)
bedrooms <- read_csv(file.path(data_dir, "bedroom_decade_overall.csv"), show_col_types = FALSE)
  
  