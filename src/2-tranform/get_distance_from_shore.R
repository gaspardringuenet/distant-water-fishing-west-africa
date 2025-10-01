library(dplyr)
library(tidyr)
library(readr)
library(raster)
library(sf)
library(ggplot2)

gfw_effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_MONTHLY_2020-2024.csv")

distance_raster <- raster("data/distance-from-shore.tif")


# Explicitly set raster CRS
crs(distance_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Convert effort to sf object
gfw_effort <- st_as_sf(gfw_effort, coords = c("lon", "lat"), crs = 4326, remove = FALSE)  # WGS84

# Extract distance values
gfw_effort$distance_to_shore <- raster::extract(distance_raster, gfw_effort) 

# Return to simple data frame 
gfw_effort <- gfw_effort |> 
  st_drop_geometry()


data <- gfw_effort |> 
  mutate(kWh = engine_power_kw_gfw * apparent_fishing_hours) |> 
  summarize(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    kWh = sum(kWh, na.rm = TRUE),
    .by = distance_to_shore
  ) |> 
  arrange(distance_to_shore) |> 
  mutate(
    h_cumulated = cumsum(apparent_fishing_hours),
    kWh_cumulated = cumsum(kWh)
  )

data |> 
  pivot_longer(cols = !distance_to_shore) |> 
  mutate(name = case_when(
    name == "apparent_fishing_hours" ~ "Apparent fishing hours",
    name == "kWh" ~ "Apparent effort (kW.h)",
    name == "h_cumulated" ~ "Cumulated hours",
    name == "kWh_cumulated" ~ "Cumulated effort (kW.h)"
  )) |> 
  ggplot(aes(x = distance_to_shore, y = value)) +
  geom_line() +
  labs(x = "Distance to shore (km)", y = "") +
  facet_wrap(~ name, scales = "free")















