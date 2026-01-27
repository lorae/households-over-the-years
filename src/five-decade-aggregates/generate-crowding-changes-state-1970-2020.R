# choropleth-states.R
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# ----------------------------
# Helper functions for transforming state geometries
# ----------------------------

# Creates a 2x2 matrix representing the rotation transformation
rot <- function(a) {
  matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
}

# Moves a state according to custom specifications
transform_state <- function(
    df, 
    state_fp, 
    rotation_angle, 
    scale_factor, 
    shift_coords
) {
  state <- df %>% filter(STATEFIP == state_fp)
  state_geom <- st_geometry(state)
  state_centroid <- st_centroid(st_union(state_geom))
  rotated_geom <- (state_geom - state_centroid) * rot(rotation_angle * pi / 180) / scale_factor + state_centroid + shift_coords
  state %>% st_set_geometry(rotated_geom) %>% st_set_crs(st_crs(df))
}

# ----------------------------
# Load and transform state geometries
# ----------------------------

# Download state shapefile from tigris
state_sf <- states(cb = TRUE, year = 2020, class = "sf") |>
  rename(STATEFIP = STATEFP) |>
  mutate(STATEFIP = as.integer(STATEFIP)) |>
  filter(!STATEFIP %in% c(60, 64, 66, 68, 69, 70, 72, 78)) |>
  st_transform(crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs") |>
  mutate(geometry = st_simplify(geometry, dTolerance = 5000))

# Transform Alaska and Hawaii
alaska <- transform_state(state_sf, 2, -39, 2.3, c(1000000, -5000000))
hawaii <- transform_state(state_sf, 15, -35, 1, c(5200000, -1400000))

# Combine all states
states_sf <- state_sf |>
  filter(!STATEFIP %in% c(2, 15)) |>
  bind_rows(alaska, hawaii)

# ----------------------------
# Read data
# ----------------------------

data_dir <- "output/five-decade-tables/raw"

hhsize_change <- read_csv(
  file.path(data_dir, "hhsize_state_change_1970_2020.csv"),
  show_col_types = FALSE
) |> filter(STATEFIP <= 56)

bedroom_change <- read_csv(
  file.path(data_dir, "bedroom_state_change_1970_2020.csv"),
  show_col_types = FALSE
) |> filter(STATEFIP <= 56)

ppbr_change <- read_csv(
  file.path(data_dir, "ppbr_state_change_1970_2020.csv"),
  show_col_types = FALSE
) |> filter(STATEFIP <= 56)

# ----------------------------
# Choropleth 1: Household size
# ----------------------------

map_hhsize <- states_sf |>
  left_join(hhsize_change, by = "STATEFIP")

p1 <- ggplot(map_hhsize) +
  geom_sf(aes(fill = change_hhsize), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    name = "Change in\nHH size\n(1970–2020)"
  ) +
  labs(
    title = "Change in Average Household Size by State, 1970–2020",
    subtitle = "Weighted mean household size",
    caption = "Source: IPUMS USA"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/choropleth_hhsize.png", p1, width = 10, height = 6, dpi = 300)

# ----------------------------
# Choropleth 2: Bedrooms (increase = blue)
# ----------------------------

map_bedroom <- states_sf |>
  left_join(bedroom_change, by = "STATEFIP")

p2 <- ggplot(map_bedroom) +
  geom_sf(aes(fill = change_bedroom), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#d73027",    # Reversed: decrease = red
    mid = "white",
    high = "#4575b4",   # Reversed: increase = blue
    midpoint = 0,
    name = "Change in\nbedrooms\n(1970–2020)"
  ) +
  labs(
    title = "Change in Average Bedrooms per Household by State, 1970–2020",
    subtitle = "Weighted mean bedrooms",
    caption = "Source: IPUMS USA"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/choropleth_bedroom.png", p2, width = 10, height = 6, dpi = 300)

# ----------------------------
# Choropleth 3: Persons per bedroom (increase = red = more crowding)
# ----------------------------

map_ppbr <- states_sf |>
  left_join(ppbr_change, by = "STATEFIP")

p3 <- ggplot(map_ppbr) +
  geom_sf(aes(fill = change_ppbr), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575b4",    # Decrease in crowding = blue
    mid = "white",
    high = "#d73027",   # Increase in crowding = red
    midpoint = 0,
    name = "Change in\nppbr\n(1970–2020)"
  ) +
  labs(
    title = "Change in Persons per Bedroom by State, 1970–2020",
    subtitle = "Weighted mean persons per bedroom",
    caption = "Source: IPUMS USA"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("output/choropleth_ppbr.png", p3, width = 10, height = 6, dpi = 300)