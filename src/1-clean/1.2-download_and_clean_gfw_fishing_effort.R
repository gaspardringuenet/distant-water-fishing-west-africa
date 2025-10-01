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
spatial_res <- "HIGH"
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
  clean_names() |> 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(eez_sf), remove = FALSE) |> 
  st_join(eez_sf |>  
            filter(POL_TYPE != "Joint regime") |>          # We keep the "strict" definition of EEZs in the case of the SEN-GNB border
            dplyr::select(TERRITORY1, ISO_TER1, geometry), 
          join = st_intersects, left = TRUE) |> 
  rename(eez_name = TERRITORY1, eez_iso3c = ISO_TER1) |> 
  st_drop_geometry()

gfw_data_old <- gfw_data_old |>
  clean_gfw_data(temporal_resolution = temp_res)

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
  clean_names() |> 
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(eez_sf), remove = FALSE) |> 
  st_join(eez_sf |>  
            filter(POL_TYPE != "Joint regime") |>
            dplyr::select(TERRITORY1, ISO_TER1, geometry), 
          join = st_intersects, left = TRUE) |> 
  rename(eez_name = TERRITORY1, eez_iso3c = ISO_TER1) |> 
  st_drop_geometry() 

gfw_data_recent <- gfw_data_recent |> 
  clean_gfw_data(temporal_resolution = temp_res)

# gfw_data_recent |>
#   summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
#             n_ids = n_distinct(vessel_id),
#             .by = c(gear_type)) |>
#   mutate(hours_percent = 100 * apparent_fishing_hours / sum(apparent_fishing_hours)) |>
#   arrange(desc(apparent_fishing_hours)) 

# bbox = st_bbox(fao34_sf)
# 
# ggplot(gfw_data_old_clean) +
#   geom_sf(data = fao34_sf, fill = "lightblue", alpha = 0.75) +
#   geom_sf(data = ne_countries()) +
#   geom_raster(aes(x = lon, y = lat, fill = apparent_fishing_hours)) +
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"])) +
#   scale_fill_viridis_c(option = "inferno",
#                        trans = "log10",
#                        na.value = NA) +
#   facet_wrap(~ gear_type)

# Save temporary files ----------------------------------------------------

write.csv(gfw_data_old,
          paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), "_TEMP.csv"),
          row.names = FALSE)

write.csv(gfw_data_recent,
          paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), "_TEMP.csv"),
          row.names = FALSE)

gfw_data_old <- read_csv(paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), "_TEMP.csv"))
gfw_data_recent <- read_csv(paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), "_TEMP.csv"))

# Add Add length, engine power and tonnage info for each obs --------------

all_vessels_x_years <- rbind(gfw_data_old, gfw_data_recent) |> 
  distinct(year, mmsi, vessel_name)


# Format our custom registry with vessels characteristics
vessels_info <- read_csv("data/vessel_info_gfw/fishing-vessels-v3.csv") |> 
  mutate(
    is_inferred_length = is.na(length_m_registry),
    is_inferred_power = is.na(engine_power_kw_registry),
    is_inferred_tonnage = is.na(tonnage_gt_registry)
  ) |> 
  dplyr::select(year, mmsi, length_m_gfw, engine_power_kw_gfw, tonnage_gt_gfw, is_inferred_length, is_inferred_power, is_inferred_tonnage, registries_listed) |>
  distinct()

vessels_info <- all_vessels_x_years |> 
  left_join(vessels_info,
            by = c("year", "mmsi"),
            relationship = "many-to-one") |> 
  relocate(vessel_name, .after = mmsi)


# Add vessels characteristics to all fishing observations
gfw_data_old <- gfw_data_old |> 
  left_join(vessels_info |> dplyr::select(year, mmsi, vessel_name, length_m_gfw, engine_power_kw_gfw, tonnage_gt_gfw),
            by = c("year", "mmsi", "vessel_name"),
            relationship = "many-to-one")

gfw_data_recent <- gfw_data_recent |> 
  left_join(vessels_info |> dplyr::select(year, mmsi, vessel_name, length_m_gfw, engine_power_kw_gfw, tonnage_gt_gfw),
            by = c("year", "mmsi", "vessel_name"),
            relationship = "many-to-one")

# Add raster information --------------------------------------------------

## Distance to shore ------------------------------------------------------

# Load GFW raster with distance to shore in km & explicitly set raster CRS to WGS84
distance_to_shore_raster <- raster("data/distance-from-shore.tif")
crs(distance_to_shore_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Convert effort to sf object with crs WGS84
gfw_data_old <- st_as_sf(gfw_data_old, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
gfw_data_recent <- st_as_sf(gfw_data_recent, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Extract distance values
gfw_data_old$distance_to_shore_km <- raster::extract(distance_to_shore_raster, gfw_data_old)
gfw_data_recent$distance_to_shore_km <- raster::extract(distance_to_shore_raster, gfw_data_recent)

## Distance from port -----------------------------------------------------

# Load GFW raster with distance from port in km & explicitly set raster CRS to WGS84
distance_from_port_raster <- raster("data/distance-from-port-v20201104.tiff")
crs(distance_from_port_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Extract distance values
gfw_data_old$distance_from_port_km <- raster::extract(distance_from_port_raster, gfw_data_old)
gfw_data_recent$distance_from_port_km <- raster::extract(distance_from_port_raster, gfw_data_recent)

## Bathymetry -------------------------------------------------------------

# Load GFW raster with distance from port in km & explicitly set raster CRS to WGS84
bathymetry_raster <- raster("data/bathymetry.tif")
crs(bathymetry_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Extract distance values
gfw_data_old$bathymetry_m <- raster::extract(bathymetry_raster, gfw_data_old)
gfw_data_recent$bathymetry_m <- raster::extract(bathymetry_raster, gfw_data_recent)

# Drop geometry / return to simple data frame 
gfw_data_old <- gfw_data_old |> st_drop_geometry()
gfw_data_recent <- gfw_data_recent |> st_drop_geometry()


# Save data ---------------------------------------------------------------

write.csv(gfw_data_old,
          paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), ".csv"),
          row.names = FALSE)

write.csv(gfw_data_recent,
          paste0(data_save_path, "gfw_effort_FAO34_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), ".csv"),
          row.names = FALSE)

# write.csv(vessels_info,
#           paste0(data_save_path, "gfw_vessels_characteristics_FAO34.csv"),
#           row.names = FALSE)
