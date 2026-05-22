# stacked-bar-hhsize-contributions-members

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

hhsize_contrib <- read_csv(
  "five-decade-aggregates/output/raw/hhsize_contributions_members.csv",
  show_col_types = FALSE
)

p <- hhsize_contrib |>
  filter(!is.na(decade_label), decade_label != "NA-1970") |>
  select(
    decade_label,
    n_child_diff,
    n_spouse_diff,
    n_other_subfamily_members_diff
  ) |>
  pivot_longer(
    cols = -decade_label,
    names_to = "component",
    values_to = "value"
  ) |>
  filter(!is.na(value)) |>
  mutate(
    component = recode(
      component,
      n_child_diff = "Change in children",
      n_spouse_diff = "Change in spouses",
      n_other_subfamily_members_diff = "Change in other household members"
    ),
    component = factor(
      component,
      levels = c(
        "Change in children",
        "Change in spouses",
        "Change in other household members"
      )
    )
  ) |>
  ggplot(aes(x = decade_label, y = value, fill = component)) +
  geom_col(color = "black", linewidth = 0.25) +
  scale_fill_manual(
    values = c(
      "Change in children" = "#fe7f2d",
      "Change in spouses" = "#fcca46",
      "Change in other household members" = "#233d4d"
    )
  ) +
  labs(
    x = "Decade",
    y = "Change in household size (persons)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
    legend.position = "bottom"
  )

ggsave(
  filename = "five-decade-aggregates/output/stacked-bar-hhsize-contributions-members.png",
  plot = p,
  width = 6.5,
  height = 5,
  units = "in",
  dpi = 300
)

p_presentation <- p +
  labs(x = NULL, y = "Change\nin number\nof household\nmembers") +
  scale_fill_manual(
    values = c(
      "Change in children" = "#fe7f2d",
      "Change in spouses" = "#fcca46",
      "Change in other household members" = "#233d4d"
    ),
    labels = c(
      "Change in children" = "Children",
      "Change in spouses" = "Spouses",
      "Change in other household members" = "Other"
    )
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_minimal(base_size = 22) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 22),
    axis.title.y = element_text(
      angle = 0,
      vjust = 0.5,
      hjust = 1,
      margin = margin(r = 10),
      size = 22
    ),
    legend.position = "right",
    legend.text = element_text(size = 20, margin = margin(l = 6)),
    legend.title = element_text(size = 20),
    legend.key.height = unit(1.2, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.spacing.y = unit(0.6, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  filename = "five-decade-aggregates/output/stacked-bar-hhsize-contributions-members-presentation.png",
  plot = p_presentation,
  width = 15,
  height = 7,
  units = "in",
  dpi = 300
)

