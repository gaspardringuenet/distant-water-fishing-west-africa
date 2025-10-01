library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)


fig_save_path <- "output/figures/eu_fishing_without_authorisation/"


# Functions ---------------------------------------------------------------

source("src/0-utils/gfwr_map_utils.R")

# Load data ---------------------------------------------------------------

eu_fishing_in_agreements <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_EU_vessels_within_FPA_authorisations_DAILY_2022-2024.csv")
eu_fishing_out_of_agreements <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_EU_vessels_out_of_FPA_authorisations_DAILY_2022-2024.csv")

eez_shp <- st_read("data/shapefiles/EEZ/eez_v11.shp") |> 
  filter(ISO_TER1 == "MRT") |> 
  st_transform(crs = st_crs(4326)) 

bbox <- st_bbox(eez_shp)



# Analysis ----------------------------------------------------------------

## Summary statistics of illegal fishing ----------------------------------

# Total fraction time spent fishing without authorisation

hours_in_agreements <- sum(eu_fishing_in_agreements$apparent_fishing_hours, na.rm = TRUE)
hours_out_of_agreements <- sum(eu_fishing_out_of_agreements$apparent_fishing_hours, na.rm = TRUE)

out_effort_fraction <- hours_out_of_agreements / (hours_out_of_agreements + hours_in_agreements)

print(paste0("EU vessels having at least one fishing authorisation in the region over the 3-year period (2022-2024) fished ", round(100*out_effort_fraction), "% of the time outside of authorised periods."))

# Statistics per EEZ and vessel

all_vessels <- sort(unique(rbind(eu_fishing_in_agreements |> distinct(vessel_name), eu_fishing_out_of_agreements |>  distinct(vessel_name))$vessel_name))
eezs <- c("MRT", "SEN", "GMB", "GNB")


total_effort_out <- eu_fishing_out_of_agreements |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(vessel_name, eez)) |> 
  complete(vessel_name = all_vessels, eez = eezs, fill = list(apparent_fishing_hours = 0)) |> 
  arrange(desc(apparent_fishing_hours))

total_effort_in <- eu_fishing_in_agreements |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(vessel_name, eez)) |> 
  complete(vessel_name = all_vessels, eez = eezs, fill = list(apparent_fishing_hours = 0)) |> 
  arrange(desc(apparent_fishing_hours))

total_effort <- total_effort_out |> 
  full_join(total_effort_in, by = join_by(vessel_name, eez)) |> 
  rename(fishing_hours_out = apparent_fishing_hours.x,
         fishing_hours_in = apparent_fishing_hours.y) |> 
  mutate(time_fraction_out = fishing_hours_out  / (fishing_hours_out  + fishing_hours_in))




total_effort |> 
  pivot_longer(cols = contains("hours")) |>
  summarize(value = sum(value, na.rm = TRUE),
            .by = c(eez, name)) |> 
  mutate(
    name = case_when(
      name == "fishing_hours_in" ~ "within authorisation",
      name == "fishing_hours_out" ~ "without authorisation",
      TRUE ~ name
      ),
    name = factor(name, levels = c("without authorisation", "within authorisation"))
  ) |> 
  ggplot(aes(x = eez, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("within authorisation" = "black", "without authorisation" = "darkred")
  ) +
  labs(
    x = "EEZ", y = "Fishing hours",
    fill = "Legal context of effort",
    title = "Legal and illegal apparent fishing effort of EU vessels in western Africa (2022-2024)."
  )
ggsave(paste0(fig_save_path, "summary_barplot.png"),
       height = 15,
       width = 20,
       units = "cm")

## Ranking the bad boys ---------------------------------------------------x

total_effort |> 
  filter(eez %in% c("MRT", "SEN")) |> 
  ggplot(aes(x = reorder_within(vessel_name, fishing_hours_out, eez), y = fishing_hours_out)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    y = "Apparent fishing effort (h)",
    x = "Vessel name",
    title = "EU vessels fishing outside of their authorisations",
    subtitle = "Time period: 2022-01-01 to 2024-12-31"
  ) +
  facet_wrap(~ eez, scales = "free") +
  scale_x_reordered(labels = function(x) gsub("___.*", "", x)) #+
#scale_y_continuous(trans = "log1p") +
#theme(axis.text.x = element_text(angle = -45))
ggsave(paste0(fig_save_path, "ranking_hours_MRT-SEN.png"),
       height = 20,
       width = 25,
       units = "cm")

total_effort |> 
  filter(!is.na(time_fraction_out), eez %in% c("MRT", "SEN")) |> 
  ggplot(aes(x = reorder_within(vessel_name, time_fraction_out, eez), y = time_fraction_out)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    y = "Fraction of the fishing time",
    x = "Vessel name",
    title = "EU vessels operating outside of their authorisations",
    subtitle = "Time period: 2022-01-01 to 2024-12-31"
  ) +
  facet_wrap(~ eez, scales = "free") +
  scale_x_reordered(labels = function(x) gsub("___.*", "", x))
ggsave(paste0(fig_save_path, "ranking_fraction_MRT-SEN.png"),
       height = 20,
       width = 25,
       units = "cm")

# Investigating one vessel
# The GOBER TERCERO is the worst in total illegal fishing time.
# She has no authorisation in MRT for 2023. Yet a substantial part of its effort (71% of the fishing time) occurs during that year.

one_vessel <- eu_fishing_out_of_agreements |> 
  mutate(year = year(time_range)) |> 
  group_by(lat, lon, vessel_name, year) |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE)) |> 
  filter(vessel_name == "GOBER TERCERO" & year == 2023)

plot_effort_map(one_vessel, eez_shp, end_date = "2022-12-31") +
  labs(title = "GOBER TERCERO's fishing effort")
