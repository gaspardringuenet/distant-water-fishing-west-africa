library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggrepel)
library(tidyr)

fig_save_path <- "output/figures/eu_vessels_authorisations/"

# Loading data ------------------------------------------------------------

eu_authorisations <- read_csv("output/data/eu_authorisations_2022-2024_clean.csv")


target_colormap <- c("Black hake" = "purple",
                     "Cephalopods" = "orange",
                     "Crustaceans" = "red",
                     "Demersal other than black hake" = "green",
                     "Shrimps" = "pink",
                     "Small pelagics" = "brown",
                     "Tuna" = "blue")



# Functions ---------------------------------------------------------------

# Function to check overlap with any previous row
check_overlap <- function(df) {
  df <- df |> 
    arrange(authorised_start_date) |> 
    mutate(overlap_flag = map_lgl(
      row_number(), 
      function(i) {
        if (i == 1) return(FALSE)
        any(
          authorised_start_date[i] < authorised_end_date[1:(i - 1)] &   # Start date is before any previous end date
          fishing_category_target[i] != fishing_category_target[1:(i - 1)] # Different fishing category target
        )
      }))
  return(df)
}

# Function to visualize the authorisations for a given eez

show_eez_authorisations <- function(eez_code = "SEN", alpha = 1, data = eu_authorisations, colormap = target_colormap) {

  all_vessels <- sort(unique(data$vessel_name)) 
  
  eez_data <- data |> 
    filter(coastal_party_rfmo %in% eez_code) 
  
  n_vessels <- length(unique(eez_data$vessel_name))

  eez_data <- eez_data |>  
    complete(vessel_name = all_vessels) |> 
    mutate(vessel_name = factor(vessel_name, levels = rev(all_vessels)))
  
  ggplot(eez_data, aes(x = authorised_start_date, xend = authorised_end_date,
                       y = vessel_name, yend = vessel_name,
                       color = fishing_category_target)) +
    geom_segment(linewidth = 3, alpha = alpha) +
    labs(
      x = "Date", y = "Vessel name", color = "Target",
      title = "EU vessels fishing authorisations",
      subtitle =  paste(paste0("Zone: ", paste(eez_code, collapse = "+"), " territorial waters"),
                        "Time range: 2022 - 2024",
                        paste0("Number of vessels: ", n_vessels),
                        sep = "\n")
    ) +
    scale_color_manual(values = colormap) +
    scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2024-12-31"))) +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
}

show_eez_authorisations()

# Function to visualize the authorisations of a given vessel

show_vessel_authorisations <- function(name, data = eu_authorisations, colormap = target_colormap){
  data |>
    filter(vessel_name == name) |> 
    arrange(authorised_start_date) |> 
    mutate(k = factor(row_number())) |> 
    ggplot(aes(x = authorised_start_date, xend = authorised_end_date,
               y = k, yend = k,
               color = fishing_category_target)) +
    geom_segment(linewidth = 3) +
    geom_label_repel(aes(label = coastal_party_rfmo), 
                     color = "black",
                     nudge_y = 1) +
    labs(
      x = "Date", y = "Authorisation number",
      color = "Target",
      title = name
    ) +
    scale_color_manual(values = colormap) +
    scale_x_date(limits = c(as.Date("2022-01-01"), as.Date("2024-12-31")))
}

# Analysis ----------------------------------------------------------------

# All authorisations

p <- show_eez_authorisations(eez_code = c("MRT", "SEN", "GMB", "GNB"), alpha = 0.4)
p
ggsave("output/figures/eu_vessels_authorisations/eu_vessels_authorisations.png",
       p,
       height = 30,
       width = 20,
       units = "cm")


# Authorisations in Mauritania

p <- show_eez_authorisations("MRT")
p
ggsave("output/figures/eu_vessels_authorisations/eu_vessels_authorisations_MRT.png",
       p,
       height = 30,
       width = 20,
       units = "cm")

# Authorisations in Senegal

p <- show_eez_authorisations("SEN")
p
ggsave("output/figures/eu_vessels_authorisations/eu_vessels_authorisations_SEN.png",
       p,
       height = 30,
       width = 20,
       units = "cm")

# Authorisations in Gambia

p  <- show_eez_authorisations("GMB")
p
ggsave("output/figures/eu_vessels_authorisations/eu_vessels_authorisations_GMB.png",
       p,
       height = 30,
       width = 20,
       units = "cm")

# Authorisations in Guinea Bissau

p  <- show_eez_authorisations("GNB")
p
ggsave("output/figures/eu_vessels_authorisations/eu_vessels_authorisations_GNB.png",
       p,
       height = 30,
       width = 20,
       units = "cm")

# Get the list of vessels which have overlapping authorisations for different targets
overlapping <- eu_authorisations |> 
  group_by(vessel_name) |> 
  group_modify(~ check_overlap(.)) |> 
  filter(any(overlap_flag))

overlapping |> 
  select(vessel_name) |> 
  distinct() |> 
  pull()

# 1 - ALFONSO RIERA TERCERO: "Crustaceans" in MRT and "Shrimps" in GNB overlapping in 2024
name <- "ALFONSO RIERA TERCERO"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 2 - CIUDAD DE HUELVA: "Crustaceans" in MRT and "Shrimps" in GNB overlapping in 2024
name <- "CIUDAD DE HUELVA"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")


# 3 - COSTA DE HUELVA: Same
name <- "COSTA DE HUELVA"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 4 - CURBEIRO:
#     - Cephalopods in GNB and Black hake in MRT in 2022
#     - Cephalopods in GNB and Black hake in SEN and GMB in 2024
name <- "CURBEIRO"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 5 - JOMAFRAN SEGUNDO: "Crustaceans" in MRT and "Shrimps" in GNB overlapping in 2024
name <- "JOMAFRAN SEGUNDO"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 6 - PEIX MAR VEINTINUEVE: Same
name <- "PEIX MAR VEINTINUEVE"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 7 - PEIX MAR VEINTIOCHO: Same
name <- "PEIX MAR VEINTIOCHO"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 8 - PRAIA DE RODEIRA: Cephalopods in GBN and Black hake in MRT in 2022
name <- "PRAIA DE RODEIRA"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 9 - RELEIXO: Same
name <- "RELEIXO"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 10 - SANTO DO MAR:
#     - Cephalopods in GNB and Black hake in MRT and GMB in 2022
#     - Cephalopods in GNB and Black hake in GMB in 2022
#     - Cephalopods in GNB and Black hake in SEN in 2023
#     - Cephalopods in GNB and Black hake in SEN in 2024
#     - Cephalopods in GNB and Black hake in GMB in 2024
name <- "SANTO DO MAR"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")

# 11 - VILLA DE MARIN: Cephalopods in GNB and Black hake in SEN in 2022
name <- "VILLA DE MARIN"
show_vessel_authorisations(name)
ggsave(paste0("output/figures/eu_vessels_authorisations/single_vessel_authorisations_", name, ".png"),
       height = 15,
       width = 20,
       units = "cm")


# The issue is that we need to know what a vessel is fishing for at a given time:
# - First, since authorisation change 

