library(openxlsx)
library(tidyverse)
library(sf)
library(janitor)


# Load shapes for maps

fao34_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |> filter(F_CODE == 34)

eez_sf <- st_read("data/shapefiles/FAO34_EEZ/eez_fao34.shp")

eez_overlap_sf <- eez_sf |>
  st_intersection(fao34_sf |> dplyr::select(geometry))


# Load the datasets

# Catch
catch <- read_csv("output/data/sau_catch/sau_catch_fao34_HS_LMEs_clean.csv") |> filter(fishing_sector == "Industrial")

# Stock evaluations
eval_cecaf <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "cecaf") |> clean_names()
eval_iccat <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "iccat") |> clean_names()

# Apparent fishing effort (total and specific to trawlers)
effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_FAO34_MONTHLY_LOW_2020-2024.csv")
trawling_effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_TRAWLING_effort_MONTHLY_LOW_2020-2023.csv")

# Identity and ownership
ownership <- read.xlsx("output/data/ownership/ownership_data_FAO34_2025-04_GFW_effort_2020-2024.xlsx",
                       sheet = "clean") |> 
  mutate(ish_name = toupper(ish_name),
         guo_name = toupper(guo_name))

# Restricted inshore areas
inshore <- read.xlsx("data/inshore_restricted_areas_FAO34.xlsx", sheet = "clean") |> 
  mutate(
    total_km = 1.852 * total_nm,
    partial_km = 1.852 * partial_nm
  )
