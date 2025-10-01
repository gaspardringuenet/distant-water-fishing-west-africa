library(readr)
library(dplyr)
library(ggplot2)
library(countrycode)

catch_gambia <- read_csv("data/catch/reconstructed_catch/SAU EEZ 270 v50-1.csv")
catch_mauritania <- read_csv("data/catch/reconstructed_catch/SAU EEZ 478 v50-1.csv")
catch_guinea_bissau <- read_csv("data/catch/reconstructed_catch/SAU EEZ 624 v50-1.csv")
catch_senegal <- read_csv("data/catch/reconstructed_catch/SAU EEZ 686 v50-1.csv")


# Data is cleaned as follows:
# - Catches in the four EEZs are combined in one dataset
# - Only catches from 'industrial' fishing is kept
# - Columns are renamed and flag iso3 code is added

catch <- rbind(
  catch_gambia,
  catch_mauritania,
  catch_guinea_bissau,
  catch_senegal
) |> 
  filter(fishing_sector == "Industrial") |>
  select(area_name, year, fishing_entity, scientific_name, common_name, functional_group, reporting_status, tonnes) |>
  rename(eez = area_name,
         flag = fishing_entity,
         reconstructed_catch_tonnes = tonnes) |> 
  mutate(
    flag_iso3 = countrycode(flag, 
                                 origin = "country.name",
                                 destination = "iso3c"),
    flag_iso3 = case_when(
      flag == "Aruba (Netherlands)" ~ "ABW",
      flag == "Unkown Fishing Country" ~ "Unkown Fishing Country",
      TRUE ~ flag_iso3
    ))

write.csv(catch,
          "output/data/sau_catch/sau_reconstructed_catch_clean.csv",
          row.names = FALSE)
