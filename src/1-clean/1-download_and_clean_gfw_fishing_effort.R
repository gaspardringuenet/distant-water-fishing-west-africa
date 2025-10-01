library(dplyr)
library(readr)
library(gfwr)
library(raster)
library(sf)

data_save_path <- "output/data/gfw_apparent_fishing_effort/"

# Functions ---------------------------------------------------------------

source("src/0-utils/gfw_import_and_cleaning_utils.R")

# Import effort from GFW API ----------------------------------------------

eezs <- c("MRT", "SEN", "GMB", "GNB")
codes_eez <- lapply(eezs, function(eez) get_region_id(region_name = eez, region_source = 'EEZ', key = Sys.getenv("GFW_TOKEN"))) |>  bind_rows()

temp_res <- "MONTHLY"
spatial_res <- "LOW"

# We separate the data in 5-year periods

## 'Old' data ranging from 2014 to 2019 -----------------------------------

years_old <- 2014:2019

gfw_data_old <- lapply(
  years_old, 
  function(year) lapply(
    codes_eez$id, 
    function(eez_id) gfw_import(
      year = year, 
      spatial_resolution = spatial_res, 
      temporal_resolution = temp_res, 
      region = eez_id, 
      region_source = 'EEZ'
    ) |>  
      mutate(
        eez_iso3c = codes_eez |> filter(id == eez_id) |> pull(iso3),
        eez_name = codes_eez |> filter(id == eez_id) |> pull(label)
        )
  )) |> 
  bind_rows() |> 
  clean_gfw_data(temporal_resolution = temp_res)

## Recent data ranging from 2020 to 2024 ----------------------------------

years_recent <- 2020:2024

gfw_data_recent <- lapply(
  years_recent, 
  function(year) lapply(
    codes_eez$id, 
    function(eez_id) gfw_import(
      year = year, 
      spatial_resolution = spatial_res, 
      temporal_resolution = temp_res, 
      region = eez_id, 
      region_source = 'EEZ'
    ) |>  
      mutate(
        eez_iso3c = codes_eez |> filter(id == eez_id) |> pull(iso3),
        eez_name = codes_eez |> filter(id == eez_id) |> pull(label)
      )
  )) |> 
  bind_rows() |>
  clean_gfw_data(temporal_resolution = temp_res)



# Save temporary files ----------------------------------------------------

write.csv(gfw_data_old,
          paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), "_TEMP.csv"),
          row.names = FALSE)

write.csv(gfw_data_recent,
          paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), "_TEMP.csv"),
          row.names = FALSE)

gfw_data_old <- read_csv(paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), "_TEMP.csv"))
gfw_data_recent <- read_csv(paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), "_TEMP.csv"))

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


# Add distance to shore and from port -------------------------------------

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

# Drop geometry / return to simple data frame 
gfw_data_old <- gfw_data_old |> st_drop_geometry()
gfw_data_recent <- gfw_data_recent |> st_drop_geometry()


# Save data ---------------------------------------------------------------

write.csv(gfw_data_old,
          paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_old), "-", max(years_old), ".csv"),
          row.names = FALSE)

write.csv(gfw_data_recent,
          paste0(data_save_path, "gfw_effort_", temp_res, "_", spatial_res, "_", min(years_recent), "-", max(years_recent), ".csv"),
          row.names = FALSE)

write.csv(vessels_info,
          paste0(data_save_path, "gfw_vessels_characteristics.csv"),
          row.names = FALSE)
