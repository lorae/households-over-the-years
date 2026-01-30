
data_cps <- read_csv("output/five-decade-tables/raw/combined_cps.csv")
data_acs <- read_csv("output/five-decade-tables/raw/hhsize_overall.csv")


library(dplyr)
library(readr)

data_cps <- read_csv("output/five-decade-tables/raw/combined_cps.csv")
data_acs <- read_csv("output/five-decade-tables/raw/hhsize_overall.csv")

combined <- full_join(
  data_acs,
  data_cps,
  by = "YEAR",
  suffix = c("_acs", "_cps")
)