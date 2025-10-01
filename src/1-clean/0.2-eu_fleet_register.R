library(bloomr.fleetregister)
library(readxl)
library(tidyverse)
library(janitor)

# Get EU vessel active in FPAs in our study zone (Mauritania, Senegal, The Gambia, Guinea-Bissau) 
eu_authorisations <- read_excel("data/Exported Authorisations.xlsx") |> 
  clean_names()

cfr_list <- eu_authorisations |> 
  select(cfr) |> 
  distinct() |> 
  pull()

uvi_list <- eu_authorisations |> 
  select(uvi) |> 
  distinct() |> 
  pull()

# Get EU fleet register
register <- bloomr.fleetregister::get_events()

# Select the vessel that were active in EU FPAs in our study zone over the period (2022-2024 included)
start <- as.POSIXct("2022-01-01", format = "%Y-%m-%d")
end <- as.POSIXct("2024-12-31", format = "%Y-%m-%d")

register_wafrica <- register |> 
  filter((!is.na(cfr) & cfr %in% cfr_list) | (!is.na(uvi) & uvi %in% uvi_list)) |> 
  filter(event_start <= end & event_end >= start) |> 
  select(
    country, 
    vessel_name,
    cfr, uvi, mmsi, 
    event, event_start, event_end, 
    vessel_type, main_fishing_gear, subsidiary_fishing_gear_1,
    loa, lbp, tonnage_gt, other_tonnage, gts,
    main_engine_power, auxiliary_engine_power
  ) |> 
  mutate(comment = NA) # To leave comments on possible modifications of the records

str(register_wafrica)

# Select the vessels that did not undergo any modification over the study period
register_nochange <- register_wafrica |>
  filter(event_start <= start, event_end >= end)  # By definition there will be only 1 observation per vessel

# Check those who underwent modifications or other events
register_changes <- setdiff(register_wafrica, register_nochange) |> 
  arrange(cfr, uvi, event_start)

register_changes_01 <- register_changes |> 
  group_by(vessel_name) |> 
  mutate(
    type_change = n_distinct(vessel_type) > 1, 
    gear_change = n_distinct(main_fishing_gear) > 1,
    gear2_change = n_distinct(subsidiary_fishing_gear_1) > 1,
    loa_change = n_distinct(loa) > 1,
    lbp_change = n_distinct(lbp) > 1,
    tonnage_gt_change = n_distinct(tonnage_gt) > 1,
    gts_change = n_distinct(gts) > 1,
    main_engine_power_change = n_distinct(main_engine_power) > 1,
    auxiliary_engine_power_change = n_distinct(auxiliary_engine_power) > 1,
      ) |> 
  filter(type_change | gear_change | gear2_change | loa_change | lbp_change | tonnage_gt_change | gts_change | main_engine_power_change | auxiliary_engine_power_change)

# We can see that only 5 vessels match these criteria
# - VILLA DE MARIN : hull length and tonnage were increased significantly BUT at the very end of the study period (14 days before 2025). New value will be disregarded.
# - CARMEN E PILAR : tonnage was increased on the 2024-10-30. Only two month. New value will be disregarded.
# - ALFONSO RIERA TERCERO : subsidiary gear type change from not known (NK) to bottom pair trawls (PTB) mid-2022. PTB will be kept.
# - ANNELIES ILENA : tonnage was increased on the 2023-01-09 by 3%. We will use the mean value.
# - Maartje Theadora : tonnage was increased on the 2023-07-10 by 1%. We will use the mean value.

# Perform final adjustments based on vessel-specific rules
register_changes <- register_changes |> 
  mutate(
    tonnage_gt = case_when(
      vessel_name == "VILLA DE MARIN" & event_start >= as.Date("2024-12-17") ~ NA_real_,
      vessel_name == "CARMEN E PILAR" & event_start >= as.Date("2024-10-30") ~ NA_real_,
      vessel_name == "ANNELIES ILENA" ~ mean(tonnage_gt, na.rm = TRUE),
      vessel_name == "Maartje Theadora" ~ mean(tonnage_gt, na.rm = TRUE),
      TRUE ~ tonnage_gt
    ),
    subsidiary_fishing_gear_1 = case_when(
      vessel_name == "ALFONSO RIERA TERCERO" & subsidiary_fishing_gear_1 == "NK" ~ "PTB",
      TRUE ~ subsidiary_fishing_gear_1
    ),
    comment = case_when(
      vessel_name == "VILLA DE MARIN" ~ "MOD event at study end: earliest values kept",
      vessel_name == "CARMEN E PILAR" ~ "MOD event: earliest values kept",
      vessel_name == "ANNELIES ILENA" ~ "1 MOD event: tonnage averaged",
      vessel_name == "Maartje Theadora" ~ "1 MOD event: tonnage averaged",
      vessel_name == "ALFONSO RIERA TERCERO" ~ "Gear change (NK → PTB): PTB retained",
      TRUE ~ NA
    )
  ) |> 
  filter(!is.na(tonnage_gt)) |>   # Remove rows where values were disregarded
  group_by(vessel_name) |> 
  filter(row_number() == 1)


# Bind the two subsets
register_wafrica_clean <- register_nochange |> 
  rbind(register_changes) |> 
  arrange(vessel_name)

# Check if we have all the authorised vessels
authorised_vessels <- eu_authorisations |> 
  select(vessel_name) |> 
  arrange(vessel_name) |>
  rename(vessel_name_x = vessel_name) |> 
  distinct() |> 
  mutate(
    vessel_name_x = str_squish(str_trim(vessel_name_x)),  # Remove extra spaces
    in_register_wafrica_clean = vessel_name_x %in% str_squish(str_trim(distinct(select(register_wafrica_clean, vessel_name))$vessel_name)),
    in_register = vessel_name_x %in% str_squish(str_trim(distinct(select(register, vessel_name))$vessel_name))
  )

# A few (7) authorised vessels are not present in the complete fleet register
# > authorised_vessels |> filter(!in_register) |> select(vessel_name_x)
# # A tibble: 7 × 1
# vessel_name_x           
# <chr>                   
# 1 "ARTIKE"                 > Involved in only one FPA in 2017. Not an issue. Also SV.
# 2 "GARBOLA"                > Active over the study period. A "Special vessel" / support vessel (SV). Might explain why.
# 3 "GIBELE"                 > SV
# 4 "HAIZEA BAT"             > SV
# 5 "HAIZEA HIRU"            > SV
# 6 "IEVA SIMONAITYTE"       
# 7 "IEVA SIMONAITYTÄ\u0096" > Last two are the same. They are not SV and they are active in FPAs. The issue lies with the name. The register actually contains records of this vessel.

# Other vessels (31) are contained in the register but not in our final clean register
# "AGURTZA BERRIA"           > EXP 2021
# "ALTARRI"                  > EXP 2017
# "AVEL VOR"                 > DES 2019
# "BALAMIDA"                 > EXP 2021
# "BELOUVE"                  > EXP 2019
# "CATRUA"                   > Name changed to "VIRGEN DE CONSOLACION" in 2019.
# "CIDADE DE FARO"           > EXP 2017
# "CIDADE DE PORTIMAO"       > EXP 2017
# "HERMANOS DELGADO"         > Name changed to "ALFONSO RIERA CUARTO" in 2017.
# "JAN MARIA"                > EXP 2018
# "LAMEIRO UNO"              > Name changed to "ISLA DE SANTA" in 2015.
# "MAARTJE THEADORA"         > Vessel name switched to low case in 2013.
# "MARSHAL NOVIKOV"          > EXP 2018
# "MARSHAL VASILEVSKIY"      > EXP 2019
# "MATXIKORTA"               > EXP 2015
# "NUEVO AMADA PRIMERO"      > Name changed to "GURE AMETZA II" in 2021.
# "NUEVO ATIS"               > Name changed to "SAJABI" in 2018.
# "PLAYA DE LOUREIRO"        > EXP 2021
# "PRAIA DE AREAMILLA"       > EXP 2019
# "SAGA"                     > RET 2018     
# "VIA HARMATTAN"            > EXP 2016
# "VIA-EUROS"                > Name changed to "VIA EUROS" in 2019. Also EXP 2022 (but visible in register_wafrica_clean)
# "VINCAS KUDIRKA"           > EXP 2018  
# "ZAHARA DOS"               > RET 2013. However, in FPAs until 2016 as SV. Possibly IMO 9292333 currently under Panamanian flag. 
# "ZAHARA TRES"              > RET 2013. IMO 9292735 (Panama). SV
# "ZAHARA UNO"               > RET 2013. IMO 9292321 (Panama). SV ?
# "ZALA"                     > Name changed to "ZILLARRI" in 2015. SV ?  
# "ZILLARRI"                 > EXP 2017. SV
# "ΔΗΜΗΤΡΙΟΣ"                > DES 2016
# "ΔΗΜΗΤΡΙΟΣ Ι"              > RET 2020      
# "ΜΑΡΙΑ Α"                  > RET 2021

# Our register is now officially valid. 
# It contains information from the EU fleet register such as ids, length, power and gear type for all vessels involved in EU FPAs during the study period and in the study zone.

register_wafrica_clean <- register_wafrica_clean |> 
  select(-c(event, event_start, event_end))

write.csv(register_wafrica_clean, "output/data/register_2022-2024_clean.csv")
