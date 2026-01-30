# line-household-us-foreign-born
library(ggplot2)
library(dplyr)
library(readr)


hhsize_birthplace <- readr::read_csv(
  "output/five-decade-tables/raw/hhsize_birthplace.csv",
  show_col_types = FALSE
)

p <- ggplot(
  hhsize_birthplace,
  aes(
    x = YEAR,
    y = hhsize,
    color = birthplace,
    linetype = birthplace,
    group = birthplace
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(hhsize_birthplace$YEAR)) +
  scale_color_manual(
    values = c(
      "U.S.-born" = "black",
      "foreign-born" = "grey60"
    ),
    labels = c(
      "U.S.-born" = "U.S.-born",
      "foreign-born" = "Foreign-born"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "U.S.-born" = "solid",
      "foreign-born" = "dashed"
    ),
    labels = c(
      "U.S.-born" = "U.S.-born",
      "foreign-born" = "Foreign-born"
    )
  ) +
  labs(
    x = NULL,
    y = "Household Size",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() + 
  theme(legend.position = "bottom")

p


ggsave(
  "output/five-decade-tables/lines_hhsize_birthplace_year.jpeg",
  p,
  width = 7,
  height = 5,
  dpi = 300
)