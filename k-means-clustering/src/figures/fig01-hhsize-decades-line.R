# fig01-hhsize-decades-line
# 
# Plot average person-level household size over the decades in aggregate.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("readr")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

hhsize_decade <- crosstab_mean(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  value = "NUMPREC",
  wt_col = "PERWT",
  group_by = c("YEAR")
) 

fig01 <- ggplot(hhsize_decade, aes(x = YEAR, y = weighted_mean)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  labs(
    title = "Average Household Size in the United States, 1900–2023",
    x = NULL,
    y = "Persons per Household",
    caption = "Note: Average household size is measured at the individual level. So, for example, the value 4.17 in 1970 indicates that the 
average American lived in a household with 4.17 persons during that time."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0) 
  )

fig01

# ----- Step 2: Save figure & data ----- #

write_csv(
  hhsize_decade,
  "k-means-clustering/output/figure-data/fig01-hhsize-decades-line.csv"
)

ggsave(
  filename = "k-means-clustering/output/figures/fig01-hhsize-decades-line.jpeg",
  plot = fig01,
  width = 6,
  height = 4,
  dpi = 500,
  scale = 1.5
)
