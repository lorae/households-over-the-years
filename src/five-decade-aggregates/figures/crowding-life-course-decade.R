library(dplyr)
library(readr)
library(ggplot2)
library(scales)

crowded_age <- readr::read_csv("output/five-decade-tables/raw/crowded_age.csv")

age_levels <- c("17 or younger", "18-29", "30-49", "50-65", "65 and older")

crowded_age_plot <- crowded_age_plot |>
  mutate(YEAR = factor(YEAR, levels = c(1970, 1980, 1990, 2000, 2010, 2020)))

decade_colors <- c(
  "1970" = "#264653",
  "1980" = "#287271",
  "1990" = "#2a9d8f",
  "2000" = "#e9c46a",
  "2010" = "#f4a261",
  "2020" = "#e76f51"
)

ggplot(crowded_age_plot, aes(x = age_bucket, y = percent_crowded, color = YEAR, group = YEAR)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = decade_colors) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Age group", y = "% crowded", color = "Decade") +
  theme_minimal()

