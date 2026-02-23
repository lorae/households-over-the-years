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
# Read change table
# ----------------------------

data_dir <- "five-decade-aggregates/output/raw"
hhsize_change <- read_csv(
  file.path(data_dir, "hhsize_state_change_1970_2020.csv"),
  show_col_types = FALSE
)

# Drop grouped / unidentified states for mapping
hhsize_change <- hhsize_change |>
  filter(STATEFIP <= 56)

# ----------------------------
# Read ppb table
# ----------------------------
ppbr_2020 <- read_csv(
  file.path(data_dir, "ppbr_state_decade.csv"),
  show_col_types = FALSE
) |> 
  filter(YEAR == 2020)

# ----------------------------
# Choropleth: change in household size
# ----------------------------

map_data <- states_sf |>
  left_join(hhsize_change, by = "STATEFIP")

p_change <- ggplot(map_data) +
  geom_sf(aes(fill = change_hhsize), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 0,
    name = "Change in\npeople per\nhousehold,\n1970-2020"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


# ----------------------------
# Choropleth: household size 2020
# ----------------------------

map_data <- states_sf |>
  left_join(ppbr_2020, by = "STATEFIP")

p_ppbr_2020 <- ggplot(map_data) +
  geom_sf(aes(fill = weighted_mean), color = "black", linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#4575b4",
    mid = "white",
    high = "#d73027",
    midpoint = 1.19,
    name = "People per\n bedroom"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# ----------------------------
# Save both maps
# ----------------------------

ggsave(
  filename = "five-decade-aggregates/output/choropleth-state-ppbr-2020.png",
  plot = p_ppbr_2020,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "five-decade-aggregates/output/choropleth-state-change-hhsize-1970-2020.png",
  plot = p_change,
  width = 10,
  height = 6,
  dpi = 300
)
