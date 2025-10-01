library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)

divisions_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |>  clean_names()
eez_sf <- st_read("data/shapefiles/EEZ/eez_v11.shp") |> clean_names()
reported_catch <- read_csv("output/data/cecaf_fao_reported_catch_clean.csv")
reconstructed_catch <- read_csv("output/data/sau_reconstructed_catch_clean.csv")

cecaf_divisions <- reported_catch |> 
  select(division_code) |> 
  distinct() |> 
  pull()

divisions_sf <- divisions_sf |> 
  filter(f_code %in% cecaf_divisions) |> 
  select(f_code, name_en, geometry)

eez_sf <- eez_sf |> 
  filter(iso_ter1 %in% c("MRT", "SEN", "GMB", "GNB")) |> 
  select(sovereign1, iso_ter1, pol_type, geometry)

st_crs(divisions_sf) == st_crs(eez_sf)
  
ggplot() +
  geom_sf(data = divisions_sf, aes(fill = f_code)) +
  geom_sf(data = eez_sf, fill = NA, color = "black")

# All EEZs are contained in the FAO 34.3.1. division, except MRT
# MRT is two third 34.3.1. and one third 34.1.3.

# We will try to model a species catch in SEN by FRA using a linear model



catch_joined <- reconstructed_catch |> 
  left_join(reported_catch |> 
              filter(division_code == "34.3.1"),
            by = join_by(flag == flag,
                         year == year,
                         scientific_name == scientific_name))
  
