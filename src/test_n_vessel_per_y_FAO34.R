library(dplyr)
library(readr)
library(gfwr)
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)

data_save_path <- "output/data/gfw_apparent_fishing_effort/"

# Functions ---------------------------------------------------------------

source("src/0-utils/gfw_import_and_cleaning_utils.R")


# Load shapefile for FAO 34 and the corresponding EEZs --------------------

fao34_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |> filter(F_CODE == 34)

eez_sf <- st_read("data/shapefiles/FAO34_EEZ/eez_fao34.shp") |>
  st_intersection(fao34_sf |> dplyr::select(geometry))


# bbox <- st_bbox(fao34_sf)
# 
# ggplot() +
#   geom_sf(data = fao34_sf, fill = "lightblue", alpha = 0.75) +
#   geom_sf(data = eez_sf, fill = "red", alpha = 0.75) +
#   geom_sf(data = ne_countries()) +
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#           ylim = c(bbox["ymin"], bbox["ymax"]))

# Import effort from GFW API ----------------------------------------------

temp_res <- "YEARLY"
spatial_res <- "LOW"
years_old <- 2014:2019
years_recent <- 2020:2024


## 'Old' data ranging from 2014 to 2019 -----------------------------------

gfw_data_old <- lapply(
  years_old, 
  function(year)
    gfw_import(
      year = year, 
      spatial_resolution = spatial_res, 
      temporal_resolution = temp_res, 
      region = fao34_sf, 
      region_source = 'USER_SHAPEFILE'
    )
) |> 
  bind_rows() |> 
  clean_names()

## Recent data ranging from 2020 to 2024 ----------------------------------

gfw_data_recent <- lapply(
  years_recent, 
  function(year)
    gfw_import(
      year = year, 
      spatial_resolution = spatial_res, 
      temporal_resolution = temp_res, 
      region = fao34_sf, 
      region_source = 'USER_SHAPEFILE'
    )
) |> 
  bind_rows() |> 
  clean_names()

library(tidyr)
library(grafify)

data <- bind_rows(gfw_data_old, gfw_data_recent) |> 
  summarize(
    `n°MMSI` = n_distinct(mmsi, na.rm = TRUE),
    `Id. GFW` = n_distinct(vessel_id, na.rm = TRUE),
    .by = time_range
  ) |> 
  pivot_longer(cols = c(`n°MMSI`, `Id. GFW`))

data |> 
  ggplot(aes(x = time_range, y = value, color = name)) +
  geom_point() +
  geom_line() +
  scale_color_grafify() +
  labs(x = "Année", y = "Navires détectés", color = "Identifiants") +
  theme_minimal() +
  ylim(0, 1500)
