# fig03-multifam-decades-line
# 
# Plot average person-level household size over the decades in aggregate.
#
# ----- Step 0: Configuration ----- #
library("dplyr")
library("duckdb")
library("dbplyr")
library("ggplot2")
library("scales")

devtools::load_all("../demographr")

con <- dbConnect(duckdb::duckdb(), "data/db/ipums.duckdb")
ipums_person <- tbl(con, "ipums_person")

multifam_decade <- crosstab_percent(
  data = ipums_person |> filter(GQ %in% c(0,1,2)),
  wt_col = "PERWT",
  group_by = c("is_multifam", "YEAR"),
  percent_group_by = c("YEAR")
) |>
  filter(is_multifam) |>
  arrange(YEAR)

# ----- Graph ----- #
fig03 <- multifam_decade |>
  ggplot(aes(x = YEAR, y = percent)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  scale_x_continuous(breaks = seq(1900, 2025, by = 10)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "% of Americans Living in Multifamily Households",
    x = NULL,
    y = "% of Americans"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(margin = margin(r = 10))
  )

fig03

# ----- Step 3: Save data and plot ----- #

write_csv(
  multifam_decade,
  "k-means-clustering/output/figure-data/fig03-multifam-decades-line.csv"
)

ggsave(
  filename = "k-means-clustering/output/figures/fig03-multifam-decades-line.jpeg",
  plot = fig03,
  width = 6,
  height = 4,
  dpi = 500,
  scale = 1.5
)
