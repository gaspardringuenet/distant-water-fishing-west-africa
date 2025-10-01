library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)
library(lubridate)


fig_save_path <- "output/figures/effort_patterns_EU/raw_effort_MRT/"

# Functions ---------------------------------------------------------------

source("src/0-utils/gfwr_map_utils.R")

# Load data ---------------------------------------------------------------

eu_fishing_in_agreements <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_EU_vessels_within_FPA_authorisations_DAILY_2022-2024.csv") |>  filter(eez == "MRT")

eez_shp <- st_read("data/shapefiles/EEZ/eez_v11.shp") |> 
  filter(ISO_TER1 == "MRT") |> 
  st_transform(crs = st_crs(4326)) 

bbox <- st_bbox(eez_shp)


# Looking for fishing_category-dependent patterns in effort ---------------

# For the total effort over the full period

in_FPA_spatial_effort <- eu_fishing_in_agreements |> 
  group_by(lat, lon) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE))

plot_effort_map(in_FPA_spatial_effort, eez_shp) +
  labs(
    title = "Apparent effort in the MRT EEZ",
      subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31"
    )

# By fishing category over the full period
# Sum

in_FPA_spatial_effort_by_fishcat <- eu_fishing_in_agreements |> 
  group_by(lat, lon, fishing_category) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE))

plot_effort_map(in_FPA_spatial_effort_by_fishcat, eez_shp) +
  labs(
    title = "Apparent effort in the MRT EEZ by fishing category (sum)",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31"
  ) +
  facet_wrap(~ fishing_category)
ggsave(paste0(fig_save_path, "spatial_apparent_fishing_hours_MRT-2022-2024_inFPA_by_fishcat_sum.png"),
       height = 30,
       width = 30,
       units = "cm")

# Mean

in_FPA_spatial_effort_mean <- eu_fishing_in_agreements |> 
  group_by(lat, lon, mmsi, fishing_category) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE)) |> # Total effort of each vessel
  group_by(lat, lon, fishing_category) |> 
  summarize(apparent_fishing_hours = mean(apparent_fishing_hours, na.rm = TRUE)) # Average vessel effort of each category

plot_effort_map(in_FPA_spatial_effort_mean, eez_shp) +
  labs(
    title = "Apparent effort in the MRT EEZ by fishing category (mean)",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31"
  ) +
  facet_wrap(~ fishing_category)
ggsave(paste0(fig_save_path, "spatial_apparent_fishing_hours_MRT-2022-2024_inFPA_by_fishcat_mean.png"),
       height = 30,
       width = 30,
       units = "cm")


# Mean by fishing category target

in_FPA_spatial_effort_mean_target <- eu_fishing_in_agreements |> 
  group_by(lat, lon, mmsi, fishing_category_target) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE)) |> # Total effort of each vessel
  group_by(lat, lon, fishing_category_target) |> 
  summarize(apparent_fishing_hours = mean(apparent_fishing_hours, na.rm = TRUE)) # Average vessel effort of each category

plot_effort_map(in_FPA_spatial_effort_mean_target, eez_shp) +
  labs(
    title = "Apparent effort in the MRT EEZ by fishing category target (mean)",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31"
  ) +
  facet_wrap(~ fishing_category_target)
ggsave(paste0(fig_save_path, "spatial_apparent_fishing_hours_MRT-2022-2024_inFPA_by_fishcat_target_mean.png"),
       height = 30,
       width = 30,
       units = "cm")

# Sd by fishing category target

in_FPA_spatial_effort_var_target <- eu_fishing_in_agreements |> 
  group_by(lat, lon, mmsi, fishing_category_target) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE)) |> # Total effort of each vessel
  group_by(lat, lon, fishing_category_target) |> 
  summarize(effort_sd = sd(apparent_fishing_hours, na.rm = TRUE)) # Average vessel effort of each category

bbox <- st_bbox(eez_shp)

in_FPA_spatial_effort_var_target |> 
  ggplot() +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = effort_sd)) +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, color = "black", fill = NA, size = 0.2) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(title = "Variance in apparent effort in the MRT EEZ by fishing category target",
       subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31",
       fill = "Standard deviation (h)") +
  scale_fill_viridis_c(
    trans = "log10",
    na.value = "red"
    ) +
  map_theme +
  facet_wrap(~ fishing_category_target)
ggsave(paste0(fig_save_path, "spatial_apparent_fishing_hours_MRT-2022-2024_inFPA_by_fishcat_target_sd.png"),
       height = 30,
       width = 30,
       units = "cm")