library(gfwr)
library(dplyr)

key <- Sys.getenv("GFW_TOKEN")

x <- get_vessel_info(query = 659278000,
                search_type = "search")

ds <- readRDS("output/clean_data/apparent_fishing_effort.rds")

effort <- ds$clean_effort

y <- effort |> 
  distinct(vessel_id, mmsi, assigned_imo) |> 
  summarize(
    n = n(),
    .by = vessel_id
  ) |> 
  filter(n > 1)

effort |> 
  distinct(vessel_id, mmsi, assigned_imo, vessel_name, flag_iso3c) |> 
  filter(assigned_imo == 8778158)
