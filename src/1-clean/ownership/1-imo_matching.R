library(readr)
library(openxlsx)
library(dplyr)
library(stringr)


# Load data completed via the GISIS database ------------------------------

path <- "data/ownership/vessel_ranking_for_ownership_FAO34_completed.xlsx"

imo_completed_ranking <- read.xlsx(xlsxFile = path,
                                   sheet = "complete_imo")


# Select only vessel ids with 2000+hrs and clean --------------------------

imo_completed_ranking <- imo_completed_ranking |> filter(apparent_fishing_hours >= 2000)

imo_completed_ranking <- imo_completed_ranking |> 
  select(-gfw_imo) |> 
  #rename(vessel_name = gisis_vessel_name) |> 
  mutate(
    company_address_country = case_when(
      is.na(company_address_country) ~ company_nationality,   # Fill company address
      TRUE ~ company_address_country
    ),
    imo = as.numeric(imo),                                    # Numeric type for IMO
    effective_since_year = as.numeric(effective_since_year),  # Numeric type for year
    gisis_vessel_name = str_trim(gisis_vessel_name),          # Trim white spaces to avoid false differences
    registered_owner = str_trim(registered_owner)
  ) |> 
  mutate(
    imo_confidence_status = case_when(
      imo_confidence_code == 1 ~ "IMO provided by GFW",       # Explicit the confidence code
      imo_confidence_code == 2 ~ "Direct match in GISIS",
      imo_confidence_code == 3 ~ "Non-ambiguous indirect match",
      imo_confidence_code == 4 ~ "Ambiguous indirect match",
      imo_confidence_code == 5 ~ "No association"
    )
  )


# Create summary of IMO ownership -----------------------------------------

imo_ownership <- imo_completed_ranking |>
  select(vessel_id, mmsi, vessel_name, gisis_vessel_name, imo_confidence_code, imo_confidence_status, imo, registered_owner, company_imo, company_nationality, company_address_country, effective_since_year) |> 
  filter(!is.na(imo)) |>
  distinct() |> 
  arrange(company_nationality, company_imo, imo)

# Get IMO list for ORBIS search -------------------------------------------

imo_list <- imo_completed_ranking |> 
  select(imo, gisis_vessel_name) |> 
  filter(!is.na(imo)) |>
  distinct()


# Get companies list for ORBIS search -------------------------------------

owners_list <- imo_completed_ranking |> 
  summarize(n_vessels = n_distinct(imo),
            .by = c(registered_owner, company_imo, company_nationality, company_address_country)) |> 
  arrange(registered_owner)

# Save in new sheets ------------------------------------------------------

wb <- loadWorkbook("data/ownership/vessel_ranking_for_ownership_FAO34_completed.xlsx")
addWorksheet(wb,"GFW-GISIS_key")
addWorksheet(wb,"IMO_list")
addWorksheet(wb, "owners_list")
writeData(wb, "GFW-GISIS_key", imo_ownership)
writeData(wb, "IMO_list", imo_list)
writeData(wb, "owners_list", owners_list)
saveWorkbook(wb, "output/data/ownership/vessel_ranking_for_ownership_FAO34_completed_processed.xlsx", overwrite = TRUE)



# new_ranking <- gfw_effort |> 
#   select(vessel_id, apparent_fishing_hours) |>
#   left_join(imo_ownership,
#             by = "vessel_id") |> 
#   summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),     # Aggregate the effort by IMO number
#             .by = c(imo, gisis_vessel_name, registered_owner, company_imo, company_nationality, company_address_country, effective_since_year)) |> 
#   arrange(desc(apparent_fishing_hours)) |>                                          # Rank
#   mutate(
#     effort_cumulated = cumsum(apparent_fishing_hours),
#     effort_cumulated_percent = 100 * effort_cumulated / sum(apparent_fishing_hours, na.rm = TRUE)   # Compute cumulated effort
#   ) |> 
#   select(-effort_cumulated) 
# 
# new_ranking$rank <- rownames(new_ranking)
# 
# new_ranking <- new_ranking |> 
#   relocate(rank)
