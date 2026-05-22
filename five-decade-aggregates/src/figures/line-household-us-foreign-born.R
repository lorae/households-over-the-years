# line-household-us-foreign-born
library(ggplot2)
library(dplyr)
library(readr)


hhsize_birthplace <- readr::read_csv(
  "five-decade-aggregates/output/raw/hhsize_birthplace.csv",
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
  "five-decade-aggregates/output/lines_hhsize_birthplace_year.jpeg",
  p,
  width = 7,
  height = 5,
  dpi = 300
)

p_presentation <- p +
  labs(y = "Household\nsize") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 1,
      margin = margin(r = 10),
      size = 18
    ),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave(
  "five-decade-aggregates/output/lines_hhsize_birthplace_year-presentation.jpeg",
  p_presentation,
  width = 11,
  height = 6,
  dpi = 300
)