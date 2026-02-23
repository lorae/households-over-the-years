# crowding-ppbr-overall-lines.R
# Side-by-side line graphs of crowding and PPBR (overall), 1970–2020

# ----- Step 0: Packages ----- #
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(forcats)
library(readr)
options(scipen = 999)

# ----- Step 1: Import data ----- #
crowding <- read_csv(
  "five-decade-aggregates/output/raw/crowded_overall.csv",
  show_col_types = FALSE
)

ppbr <- read_csv(
  "five-decade-aggregates/output/raw/ppbr_overall.csv",
  show_col_types = FALSE
)

# ----- Step 2: Build plots ----- #
p_ppbr <- ggplot(ppbr, aes(x = YEAR, y = persons_per_bedroom)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = ppbr$YEAR) +
  scale_y_continuous(limits = c(1.0, 1.7)) +
  labs(
    title = "People per bedroom",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12)

p_crowding <- ggplot(crowding, aes(x = YEAR, y = percent_crowded)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2, color = "black") +
  scale_x_continuous(breaks = crowding$YEAR) +
  scale_y_continuous(
    limits = c(0, 16),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title = "Percentage of population in crowded households",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12)

facet_lines <- p_ppbr + p_crowding +
  plot_layout(ncol = 2)

facet_lines

# ----- Step 3: Save ----- #
ggsave(
  "five-decade-aggregates/output/facet-lines-crowding-ppbr-overall.jpeg",
  plot = facet_lines,
  width = 2000,
  height = 800,
  units = "px",
  dpi = 200
)