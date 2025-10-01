library(openxlsx)
library(tidyverse)
library(ggplot2)
library(countrycode)
library(ggalluvial)
library(patchwork)
library(sf)
library(rnaturalearth)
library(cowplot)
library(FactoMineR)
library(factoextra)
library(scales)
library(rlang)
library(units)

# Load datasets
source("src/0-utils/load_all_datasets.R")

# Helper functions
source("src/0-utils/gfwr_map_utils.R")
source("src/0-utils/clean_effort.R")
source("src/0-utils/compute_props.R")

# Clean the apparent fishing effort dataset
min_dist <- 5
min_hours <- 0

trawling_effort <- trawling_effort |> 
  mutate(region_type = case_when(
    is.na(eez_iso3c) ~ "High seas",
    TRUE ~ "EEZ"
  ))

l <- clean_effort(trawling_effort , eez_overlap_sf, min_dist, min_hours, unit = "mmsi")

total_initial_effort <- l[[1]]
discarded_effort <- l[[2]]
trawling_effort <- l[[3]]



# Visualize trawling data -------------------------------------------------

bbox <- st_bbox(fao34_sf)

trawling_effort |> 
  summarize(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    distance_trawled_km = sum(distance_trawled_km, na.rm = TRUE),
    .by = c(lat, lon, label)
  ) |> 
  pivot_longer(cols = c(apparent_fishing_hours, distance_trawled_km),
               names_to = "Variable",
               values_to = "Intensity") |> 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = Intensity)) +
  facet_grid(label ~ Variable) +
  scale_fill_viridis_c(trans = "log10", na.value = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))

# Restric to bottom trawling in 2023

trawling_effort <- trawling_effort |> 
  filter(year == 2023, label == "bottom_trawling")

trawling_effort |> 
  summarize(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    distance_trawled_km = sum(distance_trawled_km, na.rm = TRUE),
    .by = c(lat, lon)
  ) |> 
  pivot_longer(cols = c(apparent_fishing_hours, distance_trawled_km),
               names_to = "Variable",
               values_to = "Intensity") |> 
  ggplot() +
  geom_tile(aes(x = lon, y = lat, fill = Intensity)) +
  facet_wrap(~ Variable) +
  scale_fill_viridis_c(trans = "log10", na.value = NA) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))

# Match trawling with ownership -------------------------------------------

n_bottom_trawlers <- trawling_effort |> distinct(mmsi) |> nrow()

# Adding info about Atlantic Shrimpers and HISEPEC

mapping <- tibble(
  guo_name_pattern = c("ATLANTIC SHRIMPERS LTD", "HISEPEC"),
  new_guo_name = c("CORNELIS VROLIJK HOLDING B.V.", "PEREIRA GROUP"),
  new_iso3 = c("NDL", "ESP"),
) |> 
  mutate(new_guo_confidence = "A vérifier")

ownership <- ownership |> 
  rowwise() |> 
  mutate(
    matched = mapping |> 
      filter(str_detect(guo_name, guo_name_pattern)) |> 
      slice(1) |> # In case of multiple matches, take the first
      list(),
    guo_name = ifelse(nrow(matched) > 0, matched[1,]$new_guo_name, guo_name),
    guo_country_iso3 = ifelse(nrow(matched) > 0, matched[1,]$new_iso3, guo_country_iso3)
  ) |> 
  select(-matched) |> 
  ungroup()

ownership_trawling <- ownership |> 
  filter(mmsi %in% trawling_effort$mmsi) |> 
  select(-c(vessel_id, vessel_name, imo_confidence_code, imo_confidence_status, ish_confidence, guo_confidence)) |> 
  mutate(imo = case_when(
    mmsi == 657216600 ~ 8778005,   # fixing a mistake
    TRUE ~ imo
  )) |> 
  distinct() 

# duplicated_mmsis <- ownership_trawling[duplicated(ownership_trawling$mmsi), ]$mmsi
# ownership |> 
#   filter(mmsi %in% duplicated_mmsis) |> 
#   View()
# 
# duplicated_imos <- ownership_trawling[duplicated(ownership_trawling$imo), ]$imo
# ownership |> 
#   filter(imo %in% duplicated_imos) |> 
#   View()

n_identified <-ownership_trawling |> distinct(mmsi) |> nrow()
n_imos <- ownership_trawling |> distinct(imo) |> nrow()
n_with_owner <- ownership_trawling |> filter(!is.na(ish_name)) |> nrow()

data <- trawling_effort |> 
  left_join(ownership_trawling,
            by = "mmsi",
            relationship = "many-to-one")
  
summary_effort_ownership <- data |>
  summarize(
    apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
    distance_trawled_km = sum(distance_trawled_km, na.rm = TRUE),
    kWh = sum(kWh, na.rm = TRUE),
    .by = c(guo_name, guo_country_iso3)
  )

# Derive trawl width from Eigaard's models --------------------------------

total_hours <- sum(data$apparent_fishing_hours, na.rm = TRUE)

no_kW_info <- data |> filter(is.na(engine_power_kw_gfw))
n_no_kW_info <- no_kW_info |> distinct(mmsi) |> nrow()
h_no_kW_info <- sum(no_kW_info$apparent_fishing_hours, na.rm = TRUE)
h_no_kW_info_percent <- 100 * h_no_kW_info / total_hours

data <- data |> 
  filter(!is.na(engine_power_kw_gfw)) |> 
  mutate(
    gear_width_model = case_when(
      ish_name == "ATLANTIC SHRIMPERS LTD" ~ "OTB_CRU",
      TRUE ~ "OTB_MIX"
    ),
    model_a = case_when(
      ish_name == "ATLANTIC SHRIMPERS LTD" ~ 37.5272,
      TRUE ~ 10.6608
    ),
    model_b = case_when(
      ish_name == "ATLANTIC SHRIMPERS LTD" ~ 0.1490,
      TRUE ~ 0.2921
    ),
    std_error_a = case_when(
      ish_name == "ATLANTIC SHRIMPERS LTD" ~ 10.6718,
      TRUE ~ 6.6939
    ),
    std_error_b = case_when(
      ish_name == "ATLANTIC SHRIMPERS LTD" ~ 0.0450,
      TRUE ~ 0.1044
    ),
    gear_width = model_a * (engine_power_kw_gfw ^ model_b),
    gear_width_minus_std_error = (model_a - std_error_a) * (engine_power_kw_gfw ^ (model_b - std_error_b)),
    gear_width_plus_std_error = (model_a + std_error_a) * (engine_power_kw_gfw ^ (model_b + std_error_b))
  )

# Compute SA (swept area) per trawler in each cell ------------------------

data <- data |> 
  mutate(
    swept_area_km2 = gear_width * 1e-3 * distance_trawled_km
  )


summary_effort_ownership <- data |>
  summarize(
    swept_area_km2 = sum(swept_area_km2),
    apparent_fishing_hours = sum(apparent_fishing_hours),
    distance_trawled_km = sum(distance_trawled_km),
    kWh = sum(kWh),
    .by = c(guo_name, guo_country_iso3)
  ) |> 
  arrange(desc(swept_area_km2)) |> 
  mutate(
    sa_percent_cumulated = 100 * cumsum(swept_area_km2) / sum(swept_area_km2)
  )

summary_effort_ownership |> 
  filter(sa_percent_cumulated <= 90.3) |> 
  ggplot(aes(x = fct_reorder(guo_name, swept_area_km2), y = swept_area_km2, fill = guo_country_iso3)) +
  geom_bar(stat = "identity") +
  coord_flip()


summary_by_guo_country <- data |>
  summarize(
    swept_area_km2 = sum(swept_area_km2),
    apparent_fishing_hours = sum(apparent_fishing_hours),
    distance_trawled_km = sum(distance_trawled_km),
    kWh = sum(kWh),
    .by = guo_country_iso3
  ) |> 
  arrange(desc(swept_area_km2)) |> 
  mutate(
    sa_percent_cumulated = 100 * cumsum(swept_area_km2) / sum(swept_area_km2),
    type = "Nationalité du GUO"
  ) |>
  rename(iso3c = guo_country_iso3)

summary_by_flag <- data |>
  summarize(
    swept_area_km2 = sum(swept_area_km2),
    apparent_fishing_hours = sum(apparent_fishing_hours),
    distance_trawled_km = sum(distance_trawled_km),
    kWh = sum(kWh),
    .by = flag_iso3c
  ) |> 
  arrange(desc(swept_area_km2)) |> 
  mutate(
    sa_percent_cumulated = 100 * cumsum(swept_area_km2) / sum(swept_area_km2),
    type = "Pavillon"
  ) |>
  rename(iso3c = flag_iso3c)

total_sa <- sum(summary_by_guo_country$swept_area_km2)
percent_na_by_guo_country <- 100 * summary_by_guo_country[is.na(summary_by_guo_country$iso3c),]$swept_area_km2 / total_sa
percent_na_by_flag <- 100 * summary_by_flag[is.na(summary_by_flag$iso3c),]$swept_area_km2 / total_sa

percent_rep_by_guo_country <- (100 / total_sa) * summary_by_guo_country |>
  #filter(!is.na(iso3c)) |> 
  head(20) |> 
  pull(swept_area_km2) |> 
  sum()

percent_rep_by_flag <- (100 / total_sa) * summary_by_flag |>
  #filter(!is.na(iso3c)) |> 
  head(20) |> 
  pull(swept_area_km2) |> 
  sum()

library(tidytext)
library(paletteer)

type1_str <- paste0("Pavillon (", round(percent_na_by_flag, 1), " % non attribuée)")
type2_str <- paste0("Nationalité du GUO (", round(percent_na_by_guo_country, 1), " % non attribuée)")

type1_str <- paste0("Pavillon (", round(percent_na_by_flag, 1), " % non attribuée; ", round(percent_rep_by_flag, 1)," % representée)")
type2_str <- paste0("Nationalité du GUO (", round(percent_na_by_guo_country, 1), " % non attribuée; ", round(percent_rep_by_guo_country, 1)," % representée)")

rbind(head(summary_by_flag, 20), head(summary_by_guo_country, 20)) |> 
  mutate(
    type = case_when(
      type == "Nationalité du GUO" ~ type2_str,
      type == "Pavillon" ~ type1_str
    ),
    type = factor(type, levels = c(type1_str, type2_str))
  ) |> 
  mutate(iso3c_reordered = reorder_within(iso3c, swept_area_km2, type)) |> 
  ggplot(aes(x = iso3c_reordered, y = swept_area_km2, fill = iso3c)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  scale_fill_paletteer_d("ggsci::default_igv",
                         na.value = "grey") +
  coord_flip() +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "", 
       y = expression(paste("Surface balayée (", {km}^2, ")"))) +
  theme_minimal() +
  theme(legend.position = "None")

data |>
  filter(is.na(guo_country_iso3)) |> 
  summarize(
    swept_area_km2 = sum(swept_area_km2),
    .by = c(flag_iso3c, guo_country_iso3)
  ) |> 
  ggplot(aes(x = fct_reorder(flag_iso3c, swept_area_km2), y = swept_area_km2, fill = flag_iso3c)) +
  geom_col() +
  coord_flip() +
  scale_fill_paletteer_d("ggsci::default_igv",
                         na.value = "grey")

# Compute Swept Area Ratio (SAR) per cell ---------------------------------

library(geosphere)

compute_cell_area <- function(lat, lon, height_degree = 0.1, width_degree = 0.1) {
  # Half the cell size in degrees
  dlat <- height_degree / 2
  dlon <- width_degree / 2
  
  # Define corners in correct order: (longitude, latitude)
  p1 <- c(lon - dlon, lat - dlat)  # lower left
  p2 <- c(lon + dlon, lat - dlat)  # lower right
  p3 <- c(lon + dlon, lat + dlat)  # upper right
  p4 <- c(lon - dlon, lat + dlat)  # upper left
  
  # Construct polygon (make sure it's closed)
  cell <- rbind(p1, p2, p3, p4, p1)
  
  # Compute area (in m²), then convert to km²
  area_km2 <- geosphere::areaPolygon(cell) / 1e6
  
  return(area_km2)
}

SAR_data <- data |> 
  rowwise() |> 
  mutate(
    cell_area_km2 = compute_cell_area(lat, lon)
  ) |> 
  ungroup() |> 
  summarize(
    cell_area_km2 = first(cell_area_km2),
    swept_area_km2 = sum(swept_area_km2),
    swept_area_ratio = swept_area_km2 / cell_area_km2,
    .by = c(lat, lon)
  )

ggplot() +
  geom_tile(data = SAR_data, aes(x = lon, y = lat, fill = swept_area_ratio)) +
  scale_fill_continuous(trans = "log1p")

# Estimate proportion of seabed trawled at least once ---------------------
# Following Amoroso et al. 2018

SAR_data <- SAR_data |> 
  mutate(
    prop_trawled_poisson = 1 - exp(-swept_area_ratio)
  )

ggplot() +
  geom_tile(data = SAR_data, aes(x = lon, y = lat, fill = prop_trawled_poisson)) +
  scale_fill_viridis_c()



