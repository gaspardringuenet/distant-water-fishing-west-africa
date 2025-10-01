library(readxl)
library(tidyverse)
library(janitor)
library(ggplot2)

start_date <- as.POSIXct("01-01-2020", format = "%d-%m-%Y")

eu_authorisations <- read_excel("data/Exported Authorisations.xlsx") |> 
  clean_names() |> 
  mutate(
    authorised_start_date = as.POSIXct(authorised_start_date, format = "%d-%m-%Y"),
    authorised_end_date = as.POSIXct(authorised_end_date, format = "%d-%m-%Y"),
    terminated_on = as.POSIXct(terminated_on, format = "%d-%m%-%Y")
  ) |> 
  filter(authorised_end_date > start_date)

eu_FPAs_fishing_categories <- read_excel("data/eu_FPAs_fishing_categories.xlsx") |> 
  clean_names() |> 
  select(code, target, gear, freezer) |> 
  mutate(freezer = (freezer == 1)) |> 
  rename(
    fishing_category_target = target,
    fishing_category_gear = gear,
    fishing_category_freezer = freezer
  )

data <- eu_authorisations |> 
  left_join(
    y = eu_FPAs_fishing_categories,
    by = join_by(fishing_category == code)
  ) |> 
  select(
    flag_state, vessel_name, cfr, uvi, ircs,
    agreement_type, coastal_party_rfmo, status,
    authorised_start_date, authorised_end_date, terminated_on,
    fishing_area,
    fishing_category, fishing_category_target, fishing_category_gear, fishing_category_freezer,
    target_species
    )

data <- data |> 
  arrange(vessel_name)

write.csv(data, 
          "output/data/eu_authorisations_2020-2024_clean.csv",
          row.names = FALSE)
