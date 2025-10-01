library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(janitor)
library(countrycode)
library(lubridate)
library(raster)

# Load raw data (trawling effort in 2020-2023 in the whole world) ---------

raw_data_list <- c(list.files(path = "data/raw_gfw_trawling_effort/12424720-19d4-11f0-beb1-9d682effc649", pattern = "\\.csv$", full.names = TRUE),
                   list.files(path = "data/raw_gfw_trawling_effort/f18b56c0-19d3-11f0-9018-5df6c30331c3", pattern = "\\.csv$", full.names = TRUE))

raw_data <- lapply(raw_data_list, read_csv) |> 
  bind_rows()

# Filter the effort occuring in the 4 EEZs --------------------------------

# Load shapefiles
fao34_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |> filter(F_CODE == 34)
eez_sf <- st_read("data/shapefiles/FAO34_EEZ/eez_fao34.shp")
eez_overlap_sf <- eez_sf |>
  st_intersection(fao34_sf |> dplyr::select(geometry))

# First filter based on bounding box (faster)
bbox <- st_bbox(fao34_sf)

filtered_data <- raw_data |> 
  filter(
    cell_ll_lon >= bbox["xmin"] & cell_ll_lon <= bbox["xmax"],
    cell_ll_lat >= bbox["ymin"] & cell_ll_lon <= bbox["ymax"]
  )

filtered_data <- filtered_data |> 
  st_as_sf(coords = c("cell_ll_lon","cell_ll_lat"), remove = FALSE, crs = st_crs(4326))

# Then based on the FAO 34 limits
filtered_data <- filtered_data |> 
  st_filter(fao34_sf)

# Join with EEZs
filtered_data <- filtered_data |> 
  st_join(eez_sf |>  
            filter(POL_TYPE != "Joint regime") |>          # We keep the "strict" definition of EEZs in the case of the SEN-GNB border
            dplyr::select(TERRITORY1, ISO_TER1, geometry), 
          join = st_intersects, left = TRUE) |> 
  st_drop_geometry()

# Clean
trawling_effort <- filtered_data |> 
  rename(
    lat = cell_ll_lat, 
    lon = cell_ll_lon,
    apparent_fishing_hours = fishing_hours,
    eez_name = TERRITORY1,
    eez_iso3c = ISO_TER1
  ) |> 
  mutate(
    time_range = ym(paste(year, month, sep = "-")),
    flag = countrycode(flag, origin = "iso3c", destination = "country.name")
  ) |>
  select(-c(month, n_mmsi, n_events)) |> 
  relocate(
    lat, 
    lon,
    time_range,
    year,
    flag,
    mmsi,
    label,
    apparent_fishing_hours,
    distance_trawled_km)

# Add Add length, engine power and tonnage info for each obs --------------

vessels_info <- read_csv("data/vessel_info_gfw/fishing-vessels-v3.csv") |> 
  dplyr::select(year, mmsi, length_m_gfw, engine_power_kw_gfw, tonnage_gt_gfw) |>
  distinct() |> 
  filter(year >= 2020, year <= 2023)

trawling_effort <- trawling_effort |>
  left_join(
    vessels_info,
    by = c("year", "mmsi"),
    relationship = "many-to-one"
  )


# Add raster information --------------------------------------------------

## Distance to shore ------------------------------------------------------

# Load GFW raster with distance to shore in km & explicitly set raster CRS to WGS84
distance_to_shore_raster <- raster("data/distance-from-shore.tif")
crs(distance_to_shore_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Convert effort to sf object with crs WGS84
trawling_effort <- st_as_sf(trawling_effort, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Extract distance values
trawling_effort$distance_to_shore_km <- raster::extract(distance_to_shore_raster, trawling_effort)

## Distance from port -----------------------------------------------------

# Load GFW raster with distance from port in km & explicitly set raster CRS to WGS84
distance_from_port_raster <- raster("data/distance-from-port-v20201104.tiff")
crs(distance_from_port_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Extract distance values
trawling_effort$distance_from_port_km <- raster::extract(distance_from_port_raster, trawling_effort)

## Bathymetry -------------------------------------------------------------

# Load GFW raster with distance from port in km & explicitly set raster CRS to WGS84
bathymetry_raster <- raster("data/bathymetry.tif")
crs(bathymetry_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Extract distance values
trawling_effort$bathymetry_m <- raster::extract(bathymetry_raster, trawling_effort)

# Drop geometry / return to simple data frame 
trawling_effort <- trawling_effort |> st_drop_geometry()


# Save trawling effort data -----------------------------------------------

write.csv(trawling_effort, 
          "output/data/gfw_apparent_fishing_effort/gfw_TRAWLING_effort_MONTHLY_LOW_2020-2023.csv",
          row.names = FALSE)

# Optional: check the spatial extent of trawling data ---------------------
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

trawling_effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_TRAWLING_effort_MONTHLY_LOW_2020-2023.csv")


data <- trawling_effort |> 
  summarize(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    distance_trawled_km = sum(distance_trawled_km, na.rm = TRUE),
    .by = c(lat, lon, label)
  ) |> 
  pivot_longer(cols = c(apparent_fishing_hours, distance_trawled_km),
               names_to = "Variable",
               values_to = "Intensity")
ggplot() +
  geom_tile(data = data, aes(x = lon, y = lat, fill = Intensity)) +
  facet_grid(label ~ Variable) +
  scale_fill_viridis_c(trans = "log10", na.value = NA)

