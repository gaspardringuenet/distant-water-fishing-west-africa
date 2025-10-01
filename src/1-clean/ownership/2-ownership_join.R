library(openxlsx)
library(janitor)
library(dplyr)
library(countrycode)

# Load data ---------------------------------------------------------------

# Key for matching a GFW vessel (vessel_id) to the vessel IMO number and the GISIS company IMO number
gfw_gisis_key <- read.xlsx("output/data/ownership/vessel_ranking_for_ownership_FAO34_completed_processed.xlsx", sheet = "GFW-GISIS_key") |> clean_names()

# Result of the ORBIS search based on IMO numbers
orbis_vessels <- read.xlsx("data/ownership/export_orbis_FAO34.xlsx", sheet = "clean_imo_results") |> clean_names()

# Result of the ORBIS search based on GISIS owner company names
orbis_companies <- read.xlsx("data/ownership/export_orbis_FAO34.xlsx", sheet = "clean_gisis_owner_results") |> clean_names()


# Join --------------------------------------------------------------------

# First we add ownership information for the vessel which were recognized by ORBIS using their IMO number
ownership_table_1 <- gfw_gisis_key |> 
  select(vessel_id, mmsi, vessel_name, imo_confidence_code, imo_confidence_status, imo) |> 
  left_join(orbis_vessels |> 
              select(starts_with(c("imo", "ish", "guo"))),
            by = "imo") |> 
  mutate(
    ish_confidence = case_when(
      !is.na(ish_name) ~ "1: Direct IMO ORBIS match",
      TRUE ~ "Other"
    )
  )

matches_1 <- ownership_table_1 |> filter(ish_confidence == "1: Direct IMO ORBIS match")

# For the rest, we try to complete using ORBIS result for the search based on GISIS company name 
ownership_table_2 <- ownership_table_1 |> 
  filter(ish_confidence == "Other") |> 
  select(vessel_id, mmsi, vessel_name, imo_confidence_code, imo_confidence_status, imo) |> 
  left_join(gfw_gisis_key |> 
              select(imo, company_imo) |> 
              filter(!is.na(imo) & !is.na(company_imo)) |> 
              distinct(),
            by = "imo") |> 
  left_join(orbis_companies |> 
              select(starts_with(c("gisis", "ish", "guo"))),
            by = join_by(company_imo == gisis_company_imo_number),
            na_matches = "never") |> 
  select(-c(company_imo, gisis_owner_name)) |> 
  mutate(
    ish_confidence = case_when(
      !is.na(ish_name) ~ "2: GISIS owner ORBIS match",
      TRUE ~ "Other"
    )
  )
  
matches_2 <- ownership_table_2 |> filter(ish_confidence == "2: GISIS owner ORBIS match")

# For the vessels we could not match in ORBIS, we use the GISIS owner as ISH
ownership_table_3 <- ownership_table_2 |> 
  filter(ish_confidence == "Other") |> 
  select(vessel_id, mmsi, vessel_name, imo_confidence_code, imo_confidence_status, imo) |> 
  left_join(gfw_gisis_key |> 
              distinct(imo, registered_owner, company_nationality) |> 
              filter(!grepl("Rptd", registered_owner)),                    # When GISIS mentions an unknown owner, we encode it as NA instead of adding poor quality info on nationality
            by = "imo") |> 
  mutate(
    company_nationality = case_when(
      company_nationality == "Azores" ~ "Portugal",
      company_nationality == "Canary Islands" ~ "Spain",
    ),
    company_nationality = countrycode(company_nationality, origin = "country.name", destination = "iso2c")
  ) |> 
  rename(ish_name = registered_owner, ish_country_iso2 = company_nationality) |> 
  mutate(
    ish_confidence = case_when(
      !is.na(ish_name) ~ "3: GISIS owner as ISH",
      TRUE ~ "4: No known ISH"
    )
  )
  

# Combine all the vessels

ownership_data <- bind_rows(matches_1, matches_2, ownership_table_3) |> 
  mutate(
    ish_country_iso2 = countrycode(ish_country_iso2, origin = "iso2c", destination = "iso3c"),
    guo_country_iso2 = countrycode(guo_country_iso2, origin = "iso2c", destination = "iso3c")
  ) |> 
  rename(
    ish_country_iso3 = ish_country_iso2,
    guo_country_iso3 = guo_country_iso2
  )

# Lastly, for the vessels with an ISH but no GUO, we use the ISH as GUO

# Flag the GUO confidence
ownership_data <- ownership_data |> 
  mutate(
    guo_confidence = case_when(
      !is.na(guo_name) ~ "1: GUO found by ORBIS",
      !is.na(ish_name) ~ "2: ISH used as GUO",
      TRUE ~ "3: No known GUO"
    )
  )

# Complete GUO with ISH
ownership_data <- ownership_data |> 
  mutate(
    guo_name = case_when(is.na(guo_name) ~ ish_name, TRUE ~ guo_name),
    guo_country_iso3 = case_when(is.na(guo_country_iso3) ~ ish_country_iso3, TRUE ~ guo_country_iso3),
    guo_type = case_when(is.na(guo_type) ~ ish_type, TRUE ~ guo_type)
  )

# Add total confidence level
ownership_data <- ownership_data |> 
  mutate(
    total_confidence = case_when(
      ish_confidence == "1: Direct IMO ORBIS match" & guo_confidence == "1: GUO found by ORBIS" ~ "High",
      ish_confidence == "2: GISIS owner ORBIS match" & guo_confidence == "1: GUO found by ORBIS" ~ "High",
      ish_confidence == "2: GISIS owner ORBIS match" & guo_confidence == "2: ISH used as GUO" ~ "Medium",
      ish_confidence == "3: GISIS owner as ISH" & guo_confidence == "2: ISH used as GUO" ~ "Low",
      TRUE ~ NA
    ),
    total_confidence = factor(total_confidence, levels = c("Low", "Medium", "High"))
  )

# Save ownership data -----------------------------------------------------
write.xlsx(ownership_data,
           "output/data/ownership/ownership_data_FAO34_2025-04_GFW_effort_2020-2024.xlsx",
           as.table = TRUE)
