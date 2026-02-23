library(dplyr)
library(readr)
library(ggplot2)
library(scales)

# Read data
crowded_age <- readr::read_csv("five-decade-aggregates/output/raw/crowded_age.csv")

age_levels <- c("17 or younger", "18-29", "30-49", "50-65", "65 and older")

crowded_age_plot <- crowded_age |>
  mutate(
    age_bucket = factor(age_bucket, levels = age_levels),
    YEAR = factor(YEAR, levels = c(1970, 1980, 1990, 2000, 2010, 2020))
  )

decade_colors <- c(
  "1970" = "#264653",
  "1980" = "#287271",
  "1990" = "#2a9d8f",
  "2000" = "#e9c46a",
  "2010" = "#f4a261",
  "2020" = "#e76f51"
)

p <- ggplot(
  crowded_age_plot,
  aes(x = age_bucket, y = percent_crowded, color = YEAR, group = YEAR)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = decade_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    x = "Age group",
    y = "% Crowded",
    color = "Year"
  ) +
  theme_minimal()

# Ensure output directory exists
dir.create("five-decade-aggregates/output/figures", recursive = TRUE, showWarnings = FALSE)

# Save plot
ggsave(
  filename = "five-decade-aggregates/output/figures/crowded_by_age_and_decade.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

