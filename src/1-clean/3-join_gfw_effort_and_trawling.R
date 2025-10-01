library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)

gfw_effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_MONTHLY_LOW_2020-2024.csv") |> filter(year <= 2023)
trawl_effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_TRAWLING_effort_MONTHLY_LOW_2020-2023.csv")



# Summary per identified gear type / fishing technique --------------------

gfw_effort_summary <- gfw_effort |> 
  summarize(
    n_obs_per_vessel_id = n_distinct(lat, lon, vessel_id, na.rm = TRUE),
    n_obs_mmsi = n_distinct(lat, lon, mmsi, na.rm = TRUE),
    total_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    .by = gear_type
  ) |> 
  arrange(desc(total_hours))

trawl_effort_summary <- trawl_effort |> 
  summarize(
    n_obs_mmsi = n_distinct(lat, lon, mmsi, na.rm = TRUE),
    total_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    .by = label
  ) |> 
  arrange(desc(total_hours))

sum(trawl_effort$apparent_fishing_hours)
sum(filter(gfw_effort, gear_type == "Trawl")$apparent_fishing_hours)
sum(filter(gfw_effort, gear_type %in% c("Trawl", NA))$apparent_fishing_hours)

# Check that the spatial extent is similar --------------------------------

gfw_effort_2 <- gfw_effort |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(lat, lon, gear_type)) |> 
  mutate(gear_type = paste0("General:", gear_type))

trawl_effort_2 <- trawl_effort |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(lat, lon, label)) |>
  mutate(label = paste0("Trawl: ", label)) |> 
  rename(gear_type = label)

gfw_effort_2 |> 
  bind_rows(trawl_effort_2) |> 
  ggplot() +
  geom_raster(aes(x = lon,
                  y = lat,
                  fill = apparent_fishing_hours)) +
  coord_sf() +
  scale_fill_viridis_c(option = "inferno", trans = "log10", na.value = NA) +
  facet_wrap(~ gear_type)

# Match by MMS and see the correspondance between classes -----------------


trawl_effort$gear_type <- "Trawl"

gfw_effort_by_mmsi <- gfw_effort |>
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(lat, lon, time_range, mmsi, gear_type))

join_table <- gfw_effort_by_mmsi[56, ] |> 
  left_join(trawl_effort,
            by = c("lat", "lon", "time_range", "mmsi"))

join_table2 <- gfw_effort_by_mmsi |> 
  right_join(trawl_effort[126831, ],
            by = c("lat", "lon", "time_range", "mmsi"))

gfw_effort |> filter(
  lon == -17.1,
  lat == 12.2,
  mmsi == 412440338,
  time_range == "2020-07-01"
) |> 
  distinct() |> 
  View()

join_table_trawl <- join_table |> 
  filter(gear_type.x == "Trawl")

