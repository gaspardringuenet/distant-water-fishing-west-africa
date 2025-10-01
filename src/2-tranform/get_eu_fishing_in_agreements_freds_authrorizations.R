library(readr)
library(dplyr)
library(lubridate)
library(gfwr)
library(tidytext) 
library(janitor)

key <- Sys.getenv("GFW_TOKEN")

data_save_path <- "output/data/gfw_apparent_fishing_effort/"

# Functions ---------------------------------------------------------------

source("src/0-utils/gfw_import_and_cleaning_utils.R")
source("src/0-utils/gfwr_map_utils.R")

# Load data ---------------------------------------------------------------

# GFW daily apparent fishing effort in 2022-2024 in MRT, SEN, GMB and GNB

eezs <- c("MRT", "SEN", "GMB", "GNB")
codes_eez <- lapply(eezs, function(eez) get_region_id(region_name = eez, region_source = 'EEZ', key = key)) |>  bind_rows()

years <- 2022:2024

gfw_data_daily <- lapply(
  years, 
  function(year) lapply(
    codes_eez$id, 
    function(eez_id) gfw_import(
      year = year, 
      spatial_resolution = 'HIGH', 
      temporal_resolution = 'DAILY', 
      region = eez_id, 
      region_source = 'EEZ'
    ) |> 
      mutate(eez = codes_eez |> filter(id == eez_id) |> pull(iso3))
  )) |> 
  bind_rows() |> 
  clean_names() |> 
  select(-c("vessel_id", "vessel_type")) |> 
  clean_gfw_data(temporal_resolution = 'DAILY')


# EU fleet register (no neeed: Fred's authorisations data have MMSIs)

register <- read_csv("output/data/register_2022-2024_clean.csv")


# Fred's authorisation database (import and clean similarly as my own)

start_date <- as.POSIXct("01-01-2022", format = "%d-%m-%Y")

eu_authorisations <- read_rds("data/authorizations_clean_fred.rds") |>  
  clean_names() |> 
  mutate(
    authorised_start_date = as.POSIXct(date_start, format = "%d-%m-%Y"),
    authorised_end_date = as.POSIXct(date_end, format = "%d-%m-%Y"),
  ) |> 
  filter(
    coastal_country %in% c("Mauritania", "Senegal", "Gambia", "Guinea-Bissau"),
    authorised_end_date > start_date
  )

# |> 
#   filter(!grepl("SV", fishing_category)) # Remove support vessels


# Select european vessels based on the authorisations MMSIs ---------

# Get MMSI numbers of the authorised EU fleet in MRT, SEN, GMB, GNB
mmsi_list <- eu_authorisations |>
  select(mmsi) |> 
  distinct() |> 
  pull()

# Filter effort data by european authorised MMSI's
gfw_data_daily <- gfw_data_daily |> 
  filter(mmsi %in% mmsi_list)

# Check cases of one mmsi for several names
gfw_data_daily |> 
  group_by(mmsi) |> 
  mutate(n_vessel_names = n_distinct(vessel_name)) |> 
  ungroup() |> 
  filter(n_vessel_names > 1) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(flag, vessel_name, mmsi, imo)) |> 
  arrange(mmsi)

gfw_data_daily |>  # Confirm that 224762000 is the only MMSI with NA as vessel_name
  filter(is.na(vessel_name)) |>
  distinct(mmsi)

gfw_data_daily |> # See the difference in info for the 2 names of 224394000
  filter(mmsi == 224394000) |> 
  select(-c(lat, lon, time_range, entry_timestamp, exit_timestamp, first_transmission_date, last_transmission_date, apparent_fishing_hours, eez)) |> 
  distinct()

# 2 IMO's have 2 different vessel_name values. Fix:
# 1. Eliminate the vessel named 'NA' (there is only one) which only accounts for 0.73 h of fishing time over 3 years.
# 2. We have 2 names for MMSI 224394000 ("RIODOMARQUINTO" and "RIODOMAR QUINTO"). Since we have already grouped the effort by lat, lon, year and mmsi,
# we can simply rename "RIODOMARQUINTO" into "RIODOMAR QUINTO" (and add the IMO), because the observations do not occur at the same location and time.

gfw_data_daily <- gfw_data_daily |>
  filter(!is.na(vessel_name)) |> # Fix n°1
  mutate(
    vessel_name = case_when(mmsi == 224394000 ~ "RIODOMAR QUINTO", TRUE ~ vessel_name), # Fix n°2
    imo = case_when(mmsi == 224394000 ~ 9317638, TRUE ~ imo)
  )


# Keep effort occuring within authorised periods for each vessel ----------

all(unique(gfw_data_daily$mmsi) %in% unique(eu_authorisations$mmsi)) # TRUE - not surprising

eu_fishing_in_agreements <- gfw_data_daily |> 
  left_join(
    eu_authorisations |> 
      distinct(mmsi, coastal_country, species_category, vessel_category, authorised_start_date, authorised_end_date) |> 
      mutate(coastal_country = countrycode(coastal_country, 
                                           origin = "country.name",
                                           destination = "iso3c"),
             mmsi = as.numeric(mmsi)), 
    by = join_by(mmsi == mmsi, eez == coastal_country),
    relationship = "many-to-many") |> 
  filter(time_range >= authorised_start_date & time_range <= authorised_end_date)

# There are many less rows in eu_fishing_in_agreements than in gfw_data_daily
# This could be the result of illegal fishing, with vessels having authorised fishing
# periods, but still fishing out of these periods.

#  Investigating "illegal" fishing from EU vessels ----------------------------

# Get all the effort out of authorised periods
eu_fishing_out_of_agreements <- gfw_data_daily |> 
  setdiff(eu_fishing_in_agreements |> 
            select(-c(species_category, vessel_category, authorised_start_date, authorised_end_date)))


# Save data  for clustering analysis ---------------------------------------

write.csv(eu_fishing_in_agreements,
          paste0(data_save_path, "gfw_effort_EU_vessels_within_FPA_authorisations_DAILY_2022-2024_FRED.csv"),
          row.names = FALSE)

write.csv(eu_fishing_out_of_agreements,
          paste0(data_save_path, "gfw_effort_EU_vessels_out_of_FPA_authorisations_DAILY_2022-2024_FRED.csv"),
          row.names = FALSE)
