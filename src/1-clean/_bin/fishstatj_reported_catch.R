library(readr)
library(dplyr)
library(janitor)

capture_quantity <- read_csv("data/catch/FI_Regional_2024/CECAF_Capture_Quantity.csv") |> clean_names()
codes_species <- read_csv("data/catch/FI_Regional_2024/CL_FI_SPECIES_GROUPS.csv") |>  clean_names()
codes_countries <- read_csv("data/catch/FI_Regional_2024/CL_FI_COUNTRY_GROUPS.csv") |>  clean_names()
codes_divisions <- read_csv("data/catch/FI_Regional_2024/CL_FI_WATERAREA_DIVISION.csv") |> clean_names()
codes_comments <- read_csv("data/catch/FI_Regional_2024/CL_FI_SYMBOL_SDMX.csv") |> clean_names()
units <- read_csv("data/catch/FI_Regional_2024/FSJ_UNIT.csv")

capture_quantity <- capture_quantity |> 
  left_join(codes_countries |> 
              select(un_code, name_en, iso3_code),
            by = join_by(country_un_code == un_code)) |> 
  select(division_code, period, name_en, iso3_code, species_alpha_3_code, value, status) |> 
  rename(year = period, 
         flag = name_en,
         flag_iso3 = iso3_code,
         reported_catch_tonnes = value) |> 
  left_join(codes_comments |> 
              select(symbol, name_en),
            by = join_by(status == symbol)) |> 
  rename(comment = name_en) |>
  select(-status) |> 
  left_join(codes_species |> 
              select(x3a_code, scientific_name, name_en),
            by = join_by(species_alpha_3_code == x3a_code)) |> 
  rename(species_code = species_alpha_3_code,
         common_name = name_en) |> 
  relocate(c(scientific_name, common_name), .before = species_code) |> 
  arrange(division_code, year, desc(reported_catch_tonnes))

write.csv(capture_quantity,
          "output/data/cecaf_fao_reported_catch_clean.csv",
          row.names = FALSE)