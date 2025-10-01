library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(patchwork)


fig_save_path <- "output/figures/effort_patterns_EU/resampled_effort_MRT/"

# Functions ---------------------------------------------------------------

source("src/0-utils/gfwr_map_utils.R")
source("src/0-utils/gridded_effort_utils.R")

# Load data ---------------------------------------------------------------

eu_fishing_in_agreements <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_EU_vessels_within_FPA_authorisations_DAILY_2022-2024.csv") |>  filter(eez == "MRT")

eez_shp <- st_read("data/shapefiles/EEZ/eez_v11.shp") |> 
  filter(ISO_TER1 == "MRT") |> 
  st_transform(crs = st_crs(4326)) 

bbox <- st_bbox(eez_shp)


# Resample the effort in a regular grid over the MRT EEZ ------------------

gridded_effort <- get_gridded_effort(shape = eez_shp,
                                     fishing_effort = eu_fishing_in_agreements,
                                     cell_size = 0.05)

gridded_effort_tot <- gridded_effort |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE), .by = cell)

p <- ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
  geom_sf(data = gridded_effort_tot, aes(fill = apparent_fishing_hours), color = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title = "Apparent fishing hours in the Mauritanian EEZ",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
    fill = "Fishing hours"
    ) +
  gradient3 +
  map_theme
p
ggsave(paste0(fig_save_path, "total_effort_0.05degree_MRT-2022-2024.png"),
       height = 15,
       width = 15,
       units = "cm",
       create.dir = TRUE)

gridded_effort_by_fishcat <- gridded_effort |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE), .by = c(cell, mmsi, fishing_category)) |> 
  summarize(
    effort_mean = mean(apparent_fishing_hours, na.rm = TRUE),
    effort_sd = sd(apparent_fishing_hours, na.rm = TRUE),
    n = n(),
    .by = c(cell, fishing_category)
    ) |> 
  mutate(
    coeff_of_var = effort_sd / effort_mean
  ) |> 
  filter(!is.na(fishing_category))

p_mean <- ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
  geom_sf(data = gridded_effort_by_fishcat, aes(fill = effort_mean), color = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title = "Average individual vessel effort per fishing category target",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
    fill = "Fishing hours"
  ) +
  gradient2 +
  map_theme + 
  facet_wrap(~ fishing_category)
p_mean
ggsave(paste0(fig_save_path, "effort_MRT-2022-2024_inFPA_by_fishcat_mean.png"),
       height = 30,
       width = 30,
       units = "cm")

p_sd <- ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
  geom_sf(data = gridded_effort_by_fishcat, aes(fill = effort_sd), color = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title = "Standard deviation of individual vessel effort per fishing category",
    subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
    fill = "Fishing hours"
  ) +
  scale_fill_viridis_c(
    trans = "log10",
    na.value = "red"
  ) +
  map_theme + 
  facet_wrap(~ fishing_category)
p_sd
ggsave(paste0(fig_save_path, "effort_MRT-2022-2024_inFPA_by_fishcat_sd.png"),
       height = 30,
       width = 30,
       units = "cm")

p_cv <- ggplot() +
  geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
  geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
  geom_sf(data = gridded_effort_by_fishcat, aes(fill = coeff_of_var), color = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  labs(
    title = "cv = sd / mean",
    #subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
    fill = "Fishing hours"
  ) +
  scale_fill_viridis_c(na.value = "red") +
  map_theme + 
  facet_wrap(~ fishing_category)
p_cv
ggsave(paste0(fig_save_path, "effort_MRT-2022-2024_inFPA_by_fishcat_cv.png"),
       height = 30,
       width = 30,
       units = "cm")


# Visualize individual patterns for each fishing_category -----------------

gridded_effort |> 
  st_drop_geometry() |> 
  dplyr::select(fishing_category) |> 
  distinct() |> 
  pull()

# ! Some mmsi's have 2 vessel_name's (or one and NA) in eu_fishing_in_agreements

plot_vessels_in_fishcat <- function(category = "MRT_TUX_LH_LLS"){
  gridded_effort |> 
    left_join(eu_fishing_in_agreements |> 
                dplyr::select(mmsi, vessel_name) |> 
                distinct(),
              by = "mmsi",
              relationship = "many-to-many") |> 
    summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE), .by = c(cell, vessel_name, fishing_category)) |> 
    filter(fishing_category == category) |> 
    ggplot() +
    geom_sf(data = ne_countries(returnclass = "sf", scale = "medium")) +
    geom_sf(data = eez_shp, fill = "lightblue", alpha = 0.3, color = "black") +
    geom_sf(aes(fill = apparent_fishing_hours), color = NA) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"])) +
    labs(
      title = paste0("Individual vessel efforts - ", category),
      subtitle = "Within EU fishing Authorisations - 2022-01-01 to 2024-12-31\n0.05° x 0.05° resampling",
      fill = "Fishing hours"
    ) +
    gradient2 +
    map_theme + 
    facet_wrap(~ vessel_name)
}

plot_vessels_in_fishcat("MRT_TUX_LH_LLS")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_TUX_LH_LLS.png"),
       height = 15,
       width = 15,
       units = "cm")

plot_vessels_in_fishcat("MRT_TUX_PS")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_TUX_PS.png"),
       height = 20,
       width = 20,
       units = "cm")

plot_vessels_in_fishcat("MRT_HKZ_OT_FROZEN")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_HKZ_OT_FROZEN.png"),
       height = 15,
       width = 25,
       units = "cm")

plot_vessels_in_fishcat("MRT_HKZ_OT_LL_FRESH")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_HKZ_OT_LL_FRESH.png"),
       height = 20,
       width = 20,
       units = "cm")

plot_vessels_in_fishcat("MRT_PEL_IND_FROZEN")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_PEL_IND_FROZEN.png"),
       height = 20,
       width = 25,
       units = "cm")

plot_vessels_in_fishcat("MRT_NRY_OT")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_NRY_OT.png"),
       height = 40,
       width = 40,
       units = "cm")

plot_vessels_in_fishcat("MRT_NRY_OT")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_NRY_OT.png"),
       height = 40,
       width = 40,
       units = "cm")

plot_vessels_in_fishcat("MRT_DEM_NTO")
ggsave(paste0(fig_save_path, "fishcat_effort_MRT-2022-2024_inFPA_per_vessel_MRT_NRY_OT.png"),
       height = 15,
       width = 25,
       units = "cm")






