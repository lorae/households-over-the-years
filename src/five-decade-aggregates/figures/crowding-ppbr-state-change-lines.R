#state-change-arrows-crowding-ppbr.R
# Connected-dot arrow plots for state-level PPBR and crowding, 1970–2020

# ----- Step 0: Packages ----- #
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(forcats)
library(readr)
options(scipen = 999)

# ----- Step 1: Import data ----- #
ppbr_state_change <- read_csv(
  "output/five-decade-tables/raw/ppbr_state_change_1970_2020.csv",
  show_col_types = FALSE
) |>
  filter(
    state_name != "State not identified",
    !is.na(ppbr_1970)
  )

crowded_state_change <- read_csv(
  "output/five-decade-tables/raw/crowded_state_change_1970_2020.csv",
  show_col_types = FALSE
) |>
  filter(
    state_name != "State not identified",
    !is.na(percent_crowded_1970)
  )

# ----- Step 2: Helpers ----- #

prep_dotplot_data <- function(data, state_order, value_prefix) {
  data_long <- data |>
    select(
      state_name,
      !!sym(paste0(value_prefix, "_1970")),
      !!sym(paste0(value_prefix, "_2020"))
    ) |>
    pivot_longer(
      cols = -state_name,
      names_to = "year",
      values_to = "observed"
    ) |>
    mutate(
      year = case_when(
        year == paste0(value_prefix, "_1970") ~ "1970",
        year == paste0(value_prefix, "_2020") ~ "2020",
        TRUE ~ year
      ),
      state_name = factor(state_name, levels = state_order)
    )
  
  band_data <- data_long |>
    distinct(state_name) |>
    mutate(row_id = row_number()) |>
    filter(row_id %% 2 == 0) |>
    mutate(ymin = row_id - 0.5, ymax = row_id + 0.5)
  
  list(data_long = data_long, band_data = band_data)
}

get_arrow_data <- function(data, state_order, value_prefix) {
  data |>
    mutate(state_name = factor(state_name, levels = state_order)) |>
    select(
      state_name,
      !!sym(paste0(value_prefix, "_1970")),
      !!sym(paste0(value_prefix, "_2020"))
    ) |>
    mutate(
      x_start = !!sym(paste0(value_prefix, "_1970")),
      x_end   = !!sym(paste0(value_prefix, "_2020")),
      direction = case_when(
        x_end > x_start ~ "increase",
        x_end < x_start ~ "decrease",
        TRUE            ~ "no_change"
      )
    )
}

make_arrowplot <- function(
    dotplot_data,
    arrow_data,
    x_title,
    limits,
    x_as_percent = FALSE,
    show_y_labels = TRUE
) {
  
  ggplot() +
    geom_rect(
      data = dotplot_data$band_data,
      aes(ymin = ymin, ymax = ymax),
      xmin = -Inf, xmax = Inf,
      fill = "grey95"
    ) +
    
    geom_segment(
      data = arrow_data,
      aes(
        x = x_start,
        xend = x_end,
        y = state_name,
        yend = state_name,
        color = direction
      ),
      arrow = arrow(length = unit(0.12, "cm")),
      linewidth = 0.5
    ) +
    
    geom_point(
      data = dotplot_data$data_long |>
        filter(year == "1970") |>
        left_join(
          arrow_data |> select(state_name, direction),
          by = "state_name"
        ),
      aes(x = observed, y = state_name, color = direction),
      size = 1.2
    ) +
    
    scale_color_manual(
      values = c(
        "increase" = "darkblue",
        "decrease" = "darkred",
        "no_change" = "gray"
      )
    ) +
    
    scale_x_continuous(
      name = x_title,
      limits = limits,
      expand = c(0, 0),
      labels = if (x_as_percent)
        scales::label_percent(accuracy = 1)
      else waiver()
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = if (show_y_labels)
        element_text(size = 9)
      else element_blank(),
      axis.text.x = element_text(size = 10),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# ----- Step 3: State ordering ----- #
state_order <- ppbr_state_change |>
  arrange(ppbr_1970) |>
  pull(state_name)

# ----- Step 4: Arrow data ----- #
arrow_ppbr <- get_arrow_data(
  ppbr_state_change,
  state_order,
  value_prefix = "ppbr"
)

arrow_crowded <- get_arrow_data(
  crowded_state_change,
  state_order,
  value_prefix = "percent_crowded"
)

# ----- Step 5: Manual arrow legend ----- #
legend_arrow_df <- tibble(
  direction = c("decrease", "increase"),
  x_start   = c(1.6, 1.6),
  x_end     = c(1.4, 1.8),
  y         = c(1, 1),
  label     = c("decrease", "increase")
)

arrow_legend_plot <- ggplot(legend_arrow_df) +
  geom_segment(
    aes(x = x_start, xend = x_end, y = y, yend = y, color = direction),
    arrow = arrow(length = unit(0.12, "cm")),
    linewidth = 0.6
  ) +
  geom_point(
    aes(x = x_start, y = y, color = direction),
    size = 1.2
  ) +
  geom_text(
    aes(
      x = x_end + ifelse(direction == "increase", 0.05, -0.05),
      y = y,
      label = label
    ),
    hjust = ifelse(legend_arrow_df$direction == "increase", 0, 1),
    size = 3.5
  ) +
  scale_color_manual(
    values = c("increase" = "darkblue", "decrease" = "darkred")
  ) +
  theme_void() +
  theme(legend.position = "none") + 
  coord_cartesian(xlim = c(1.2, 2.1))

# ----- Step 6: Build plots ----- #

p_ppbr <- make_arrowplot(
  dotplot_data = prep_dotplot_data(
    ppbr_state_change,
    state_order,
    value_prefix = "ppbr"
  ),
  arrow_data = arrow_ppbr,
  x_title = "Persons per Bedroom",
  limits = c(1, 2.5)
)

p_crowded <- make_arrowplot(
  dotplot_data = prep_dotplot_data(
    crowded_state_change,
    state_order,
    value_prefix = "percent_crowded"
  ),
  arrow_data = arrow_crowded,
  x_title = "Crowded Households",
  limits = c(0, 30),
  show_y_labels = FALSE
)

fig03 <- (p_ppbr + p_crowded) / arrow_legend_plot +
  plot_layout(heights = c(1, 0.12)) +
  plot_annotation(
    caption = "Wyoming, Vermont, South Dakota, North Dakota, Montana,\n
    Idaho, and Delaware are excluded due to lack of data in 1970."
    )

fig03

# ----- Step 7: Save ----- #
ggsave(
  "output/five-decade-tables/state-ppbr-crowding-arrows.jpeg",
  plot = fig03,
  width = 3000,
  height = 4000,
  units = "px",
  dpi = 400
)
