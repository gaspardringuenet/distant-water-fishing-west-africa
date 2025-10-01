library(countrycode)
library(dplyr)
library(gfwr)
library(janitor)
library(lubridate)
library(openxlsx)

# Function to import GFW data for a given year
gfw_import <- function(year, spatial_resolution, temporal_resolution, region, region_source){
  get_raster(spatial_resolution = spatial_resolution,
             temporal_resolution = temporal_resolution,
             group_by = "VESSEL_ID",
             start_date = paste0(year,"-01-01"),
             end_date = paste0(year,"-12-31"),
             region = region,
             region_source = region_source,
             key = Sys.getenv("GFW_TOKEN"))
}

# Function to import GFW data for given year and month
gfw_import_month <- function(year, month, spatial_resolution, temporal_resolution, region, region_source){
  if (region_source == "user_json") {
    fao_areas <- load_fao_area_shp(region)
    region <- transform_shp_to_json(fao_areas)}
  
  date <- as.Date(paste(year, month, "15", sep = "-"), format = "%Y-%m-%d")
  start_date <- floor_date(date, "month")
  end_date <- ceiling_date(date, "month") - days(1)
  
  get_raster(spatial_resolution = spatial_resolution,
             temporal_resolution = temporal_resolution,
             group_by = "VESSEL_ID",
             start_date = start_date,
             end_date = end_date,
             region = region,
             region_source = region_source,
             key = Sys.getenv("GFW_TOKEN"))
}


# Function to clean GFW data
clean_gfw_data <- function(df, temporal_resolution = "MONTHLY") {
  df |> 
    clean_names() |> 
    filter(vessel_type == "FISHING") |> 
    mutate(
      european_flag = ifelse(flag %in% pull(read.xlsx("https://www.dropbox.com/s/dp8l4nz8fmr5v10/eu27_iso_codes.xlsx?dl=1")), TRUE, FALSE),
      flag = ifelse(flag == "NULL", NA, countrycode(flag, origin =  "iso3c", destination = "country.name")),
      across(matches(c("lat", "lon", "mmsi", "imo", "apparent_fishing_hours")), ~as.numeric(.)),
      across(matches(c("flag", "vessel_name", "gear_type", "call_sign", "eez_iso3c")), ~as.character(.)),
      time_range = if (temporal_resolution == "YEARLY") {
        as.numeric(time_range)
      } else if (temporal_resolution == "MONTHLY") {
        ym(time_range)
      } else {
        ymd(time_range)
      },
      year = if (temporal_resolution == "YEARLY") {
        time_range
      } else {
        year(time_range)
      },
      imo = ifelse(nchar(imo) == 7, imo, NA),
      gear_type = case_when(
        grepl("TUNA", gear_type) ~ "Tuna purse seines",
        grepl("SEINE", gear_type) & !grepl("TUNA", gear_type) ~ "Other seines",
        grepl("SQUID", gear_type) ~ "Squid jiggers",
        grepl("TRAWL", gear_type) ~ "Trawlers",
        grepl("DREDGE", gear_type) ~ "Dredges",
        grepl("FIXED|POT|SET", gear_type) ~ "Fixed gear",
        grepl("LINE|TROLL", gear_type) ~ "Hooks and lines",
        grepl("FISHING|OTHER|GEAR|INCONCLUSIVE", gear_type) ~ "Unknown",
        TRUE ~ gear_type
      ),
      gear_type_fr = case_when(
        gear_type == "Tuna purse seines" ~ "Sennes coulissantes au thon",
        gear_type == "Other seines" ~ "Autres sennes",
        gear_type == "Squid jiggers" ~ "Turluttes",
        gear_type == "Trawlers" ~ "Chaluts",
        gear_type == "Dredges" ~ "Dragues",
        gear_type == "Fixed gear" ~ "Arts dormants",
        gear_type == "Hooks and lines" ~ "Lignes et hameçons",
        gear_type == "Unknown" ~ "Inconnus",
        TRUE ~ gear_type
      ),
      region_type = case_when(
        !is.na(eez_name) ~ "EEZ",
        TRUE ~ "High seas"
      )
    ) |> 
    dplyr::select(-c(entry_timestamp, exit_timestamp, first_transmission_date, last_transmission_date)) |> 
    relocate(
      lat, lon, 
      region_type, eez_name, eez_iso3c,
      time_range, year,
      vessel_id, vessel_name, flag, mmsi, imo, call_sign,
      apparent_fishing_hours,
      gear_type,
      european_flag
    )
}