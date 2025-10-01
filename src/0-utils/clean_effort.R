library(tidyverse)
library(countrycode)

west_africa_flags <- c("MAR",
                       "ESH",
                       "MRT",
                       "SEN",
                       "GMB",
                       "GNB",
                       "GIN",
                       "SLE",
                       "LBR",
                       "CPV",
                       "CIV",
                       "GHA",
                       "TGO",
                       "BEN",
                       "NGA",
                       "CMR",
                       "STP",
                       "GNQ",
                       "GAB",
                       "COD",
                       "COG",
                       "AGO")

clean_effort <- function(effort, 
                         eez_overlap_sf,
                         min_dist = 5, 
                         min_hours = 2000,
                         unit = c("mmsi", "vessel_id")) {
  
  unit = match.arg(unit)
  
  unit_column <- switch(unit,
                        "mmsi" = effort$mmsi,
                        "vessel_id" = effort$vessel_id)
  
  effort <- effort |> 
    mutate(unit = unit_column)
  
  # Seuil de distance au port (km) pour conserver une observation
  effort <- effort |> 
    filter(distance_from_port_km >= min_dist)
  
  total_effort <- sum(effort$apparent_fishing_hours, na.rm = TRUE)
  
  # Filtre par nombre d'heures de pêche par unit
  
  vessel_list <- effort |> 
    summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
              .by = c(unit)) |> 
    filter(apparent_fishing_hours < min_hours) |> 
    pull(unit)
  
  discarded_effort <- effort |> 
    filter(unit %in% vessel_list) |>
    summarize(total = sum(apparent_fishing_hours, na.rm = TRUE)) |> 
    pull(total)
  
  effort <- effort |> 
    filter(!(unit %in% vessel_list))
  
  # Addition de variables et flags
  
  effort <- effort |> 
    mutate(
      region_type_fr = case_when(
        region_type == "EEZ" ~ "ZEE",
        region_type == "High seas" ~ "Haute mer"
      ),
      eez_iso3c = case_when(
        eez_name == "Madeira" ~ "PRT",
        eez_name == "Azores" ~ "PRT",
        eez_name == "Canary Islands" ~ "ESP",
        TRUE ~ eez_iso3c
      ),
      flag_iso3c = countrycode(flag, origin = "country.name", destination = "iso3c"),
      flag_iso3c = case_when(
        flag == "Madeira" ~ "PRT",
        flag == "Azores" ~ "PRT",
        flag == "Canary Islands" ~ "ESP",
        TRUE ~ flag_iso3c
      ),
      kWh = apparent_fishing_hours * engine_power_kw_gfw,
      domestic = (region_type == "EEZ") & (flag_iso3c == eez_iso3c) | (eez_iso3c == "ESH") & (flag_iso3c == "MAR"),
      is_fao34_flag = flag_iso3c %in% unique(eez_overlap_sf$ISO_TER1),
      is_west_african_flag = flag_iso3c %in% west_africa_flags,
      origin_type_fr = case_when(
        is.na(flag_iso3c) ~ NA,
        domestic ~ "Domestique",
        is_west_african_flag ~ "Distante (pavillon ouest-africain)",
        TRUE ~ "Distante (autre)"
      ),
      origin_type_fr = factor(origin_type_fr, levels = c("Domestique",
                                                         "Distante (pavillon ouest-africain)",
                                                         "Distante (autre)")
      ),
      origin_type_fr = fct_explicit_na(origin_type_fr, na_level = "NA"),
      destination_type_fr = case_when(
        region_type == "High seas" ~ "Haute mer",
        domestic ~ "Domestique",
        region_type == "EEZ" & !domestic ~ "ZEE étrangère"
      )
    )
  
  list(total_effort, discarded_effort, effort)
}