library(openxlsx)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(stringr)
library(cowplot)
library(forcats)
library(grid)
library(patchwork)  

source("src/0-utils/sau_stock_status_utils.R")
source("src/0-utils/official_stock_status_utils.R")

cecaf <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "cecaf_clean")  |> clean_names()
iccat <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "iccat_clean") |> clean_names()

catch <- read_csv("output/data/sau_catch/sau_catch_fao34_HS_LMEs_clean.csv") |> 
  filter(fishing_sector == "Industrial")


# Label catch

# Official assessments (CECAF, ICCAT)
catch <- match_cecaf_assessment(catch, cecaf)
catch <- match_iccat_assessment(catch, iccat)

catch |> 
  distinct(functional_group) |> 
  print(n = 26)

# ggplot(catch, )
# 
# catch |>
#   mutate(
#     type = case_when(
#       functional_group %in% c("Large pelagics (>=90 cm)",
#                               ""
#                               "Large sharks (>=90 cm)",
#                               "") ~ "Large pelagics"
#     )
#   )



# Historical catch by functional groups -----------------------------------

new_groups <- catch |> 
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    .by = functional_group
  ) |> 
  arrange(desc(tonnes)) |> 
  mutate(
    cumulated_percent = 100 * cumsum(tonnes) / sum(tonnes),
    functional_group2 = case_when(
      cumulated_percent < 98 ~ functional_group,
      TRUE ~ NA
    )
  )

catch2 <- catch |> 
  left_join(new_groups |> select(functional_group, functional_group2),
            by = "functional_group") 

catch_timeseries <- catch2 |>
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    .by = c(year, area_name, functional_group2)
  ) |>
  group_by(functional_group2) |>
  mutate(total = sum(tonnes, na.rm = TRUE)) |>
  ungroup() |> 
  mutate(
    functional_group2 = fct_reorder(functional_group2, total),
    functional_group2 = fct_explicit_na(functional_group2, "Other")
  )


ggplot(catch_timeseries,
       aes(x = year, y = tonnes,
           fill = functional_group2, total)) +
  geom_area(color = "black", linewidth = .1) +
  facet_wrap(~ area_name) +
  scale_fill_brewer(palette = "Set3",
                    na.value = "grey50") +
  theme_minimal() +
  labs(x = "", y = "Captures (t)",
       fill = "Groupes fonctionnels SAU")



# Catch status summary 2014-2019 ------------------------------------------

catch <- catch |> 
  filter(year >= 2015, year <= 2019) |> 
  mutate(
    official_status = case_when(
      !is.na(cecaf_assessment_fr) ~ str_trim(cecaf_assessment_fr),
      overfished == 1 & overfishing == 1 ~ "Surpêché et dégradé",
      overfished == 1 ~ "Dégradé",
      overfishing == 1 ~ "Surpêché",
      overfished == 0 & overfishing == 0 ~ "Ni dégradé ni surpêché",
      TRUE ~ "Non évalué"
    ),
    dim_assessment = case_when(
      !is.na(cecaf_assessment_fr) ~ 1,
      !is.na(overfished) | !is.na(overfishing) ~ 2,
      TRUE ~ 0
    ),
    official_status = factor(
      official_status,
      levels = c("Ni dégradé ni surpêché",
                 "Non pleinement exploité",
                 "Principalement non pleinement exploité",
                 "Pleinement exploité",
                 "Principalement pleinement exploité",
                 "Principalement surexploité",
                 "Surpêché",
                 "Dégradé",
                 "Surpêché et dégradé",
                 "Surexploité",
                 "Principalement non évalué",
                 "Non évalué")
    )
  )

labelled_catch_summary <- catch |> 
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    .by = c(year, official_status, cecaf_assessment_fr, dim_assessment)
  )


colorscale <- c("Ni dégradé ni surpêché" = "darkgreen",
                "Non pleinement exploité" = "darkgreen",
                "Principalement non pleinement exploité" = "greenyellow",
                "Pleinement exploité" = "yellow",
                "Principalement pleinement exploité" = "orange",
                "Principalement surexploité" = "red",
                "Surpêché" = "orange",
                "Dégradé" = "red",
                "Surpêché et dégradé" = "darkred",
                "Surexploité" = "darkred",
                "Principalement non évalué" = "lightgrey",
                "Non évalué" = "grey")


p_cecaf <- labelled_catch_summary |> 
  filter(dim_assessment == 1) |> 
  ggplot(aes(x = year, y = tonnes, fill = official_status)) +
  geom_col() +
  scale_fill_manual(values = colorscale) +
  ylim(0, 4.5e6) +
  labs(x = "",
       y = "Captures (t)",
       fill = "Evaluations COPACE (a)") +
  theme_minimal()
    
p_iccat <- labelled_catch_summary |> 
  filter(dim_assessment == 2) |> 
  ggplot(aes(x = year, y = tonnes, fill = official_status)) +
  geom_col() +
  scale_fill_manual(values = colorscale) +
  ylim(0, 4.5e6) +
  labs(x = "",
       y = "Captures (t)",
       fill = "Evaluations CICTA (b)") +
  theme_minimal()

p_other <- labelled_catch_summary |> 
  filter(dim_assessment == 0) |> 
  ggplot(aes(x = year, y = tonnes, fill = official_status)) +
  geom_col() +
  scale_fill_manual(values = colorscale) +
  ylim(0, 4.5e6) +
  labs(x = "",
       y = "Captures (t)",
       fill = "Taxons non évalués\npar les commissions (c)") +
  theme_minimal()

legend_cecaf <- get_legend(p_cecaf + theme())
legend_iccat <- get_legend(p_iccat + theme())
legend_other <- get_legend(p_other + theme())

legend <- plot_grid(
  legend_cecaf,
  plot_grid(
    legend_iccat,
    legend_other,
    ncol = 1
  ),
  ncol = 2,
  align = "h",
  axis = "t"
) +
  theme(legend.box.margin = margin(40, 0, 40, 0))

squeezed_legend <- ggdraw() + 
  draw_grob(
    grid::editGrob(ggplotGrob(legend), vp = grid::viewport(height = unit(0.5, "npc")))
  )

p <- plot_grid(
  p_cecaf + theme(legend.position = "None"),
  p_iccat + theme(legend.position = "None"),
  p_other + theme(legend.position = "None"),
  squeezed_legend,
  labels = c("a", "b", "c", "")
)

p

# % of catch that is overexploited, not overexploited or not asses --------
# By flag and flotilla

# Categories :
# - 1: "Au moins un indice de surexploitation (stock surexploité, dégradé ou surpêché) pour la majorité des captures ("Principalement...")
# - 2: "Aucun indice de surexploitation pour la majorité des captures"
# - 3: "Pas d'information pour la majorité des captures"

indices_summary <- catch |> 
  filter(year >= 2015, year <= 2019) |> 
  mutate(
    overexploitation_flag = case_when(
      official_status %in% c("Surexploité", "Principalement surexploité", "Dégradé", "Surpêché", "Surpêché et dégradé") ~ "Niveau 1",
      official_status %in% c("Non pleinement exploité", "Principalement non pleinement exploité", "Pleinement exploité", "Principalement pleinement exploité") ~  "Niveau 2",
      official_status %in% c("Non évalué", "Principalement non évalué") ~  "Niveau 3"
    ),
    niveau_1 = overexploitation_flag == "Niveau 1",
    niveau_2 = overexploitation_flag == "Niveau 2",
    niveau_3 = overexploitation_flag == "Niveau 3"
  ) |> 
  summarize(
    tonnes_1 = sum(tonnes * niveau_1, na.rm = TRUE),
    tonnes_2 = sum(tonnes * niveau_2, na.rm = TRUE),
    tonnes_3 = sum(tonnes * niveau_3, na.rm = TRUE),
    .by = c(
      fishing_entity #, 
      # gear_type
      )
  ) |> 
  mutate(
    flotilla = fishing_entity, # paste0(fishing_entity, " - ", gear_type),
    tonnes = tonnes_1 + tonnes_2 + tonnes_3
  ) |> 
  arrange(desc(tonnes)) |> 
  mutate(tonnes_cumulated_percent = 100 * cumsum(tonnes) / sum(tonnes)) |> 
  filter(tonnes_cumulated_percent <= 90) |> 
  pivot_longer(cols = c(tonnes_1, tonnes_2, tonnes_3),
               values_to = "tonnes_by_level") |> 
  mutate(
    overexploitation_flag = case_when(
      name == "tonnes_1" ~ "Surexploitation probable",
      name == "tonnes_2" ~ "Non surexploitation probable",
      name == "tonnes_3" ~ "Information lacunaire"
    )
  )


p1 <- ggplot(indices_summary, aes(x = fct_reorder(flotilla, tonnes), y = tonnes_by_level / 5, fill = overexploitation_flag)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(
    values = c("Surexploitation probable" = "red",
               "Non surexploitation probable" = "green3",
               "Information lacunaire" = "grey")
  ) +
  labs(y = "Captures reconstruites (t.an-1)",
       x = "",
       fill = "Niveau de surexploitation") +
  theme_minimal() +
  theme(legend.position = "left")
# +
  # facet_wrap(~ area_name)

# Compare with apparent fishing effort 2015-2019 --------------------------
library(sf)

source("src/0-utils/clean_effort.R")

effort <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_FAO34_MONTHLY_LOW_2014-2019.csv")
fao34_sf <- st_read("data/shapefiles/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp") |> filter(F_CODE == 34)
eez_sf <- st_read("data/shapefiles/FAO34_EEZ/eez_fao34.shp")
eez_overlap_sf <- eez_sf |>
  st_intersection(fao34_sf |> dplyr::select(geometry))

l <- clean_effort(effort, eez_overlap_sf, min_dist = 5, min_hours = 2000)
effort <- l[[3]]

p2 <- effort |> 
  filter(year >= 2015) |> 
  mutate(flotilla = paste0(flag_iso3c)) |> 
  summarize(
    kWh = sum(kWh, na.rm = TRUE),
    .by = flotilla
  ) |> 
  arrange(desc(kWh)) |> 
  mutate(
    cumulated_percent = 100 * cumsum(kWh) / sum(kWh)
  ) |> 
  filter(cumulated_percent <= 90) |> 
  ggplot(aes(x = fct_reorder(flotilla, kWh), y = kWh)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_minimal()

p1 | p2

# Check raw catch ---------------------------------------------------------

catch_fao34_raw <- read_csv("data/catch/reconstructed_catch/SAU FAO 34 v50-1/SAU FAO 34 v50-1_fixed.csv")

p3 <- catch_fao34_raw |> 
  filter(fishing_sector == "Industrial", year >= 2015) |> 
  mutate(is_MAR_PS = (fishing_entity == "Morocco") & (gear_type == "purse seine")) |> 
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    .by = c(year, is_MAR_PS)
  ) |> 
  ggplot(aes(x = year, y = tonnes, fill = is_MAR_PS)) +
  geom_area() +
  theme_minimal()

p3.1 <- catch_fao34_raw |> 
  filter(fishing_sector == "Industrial") |> 
  mutate(is_MAR_PS = (fishing_entity == "Morocco") & (gear_type == "purse seine")) |> 
  filter(is_MAR_PS) |> 
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    .by = c(year, scientific_name)
  ) |> 
  ggplot(aes(x = year, y = tonnes, fill = scientific_name)) +
  geom_area() +
  theme_minimal()

p3.1

p4 <- effort |> 
  filter(year >= 2015) |> 
  mutate(is_MAR_S = (flag_iso3c == "MAR") & (gear_type %in% c("Tuna purse seines", "Other seines"))) |> 
  summarize(
    kWh = sum(kWh, na.rm = TRUE),
    .by = c(year, is_MAR_S)
  ) |> 
  ggplot(aes(x = year, y = kWh, fill = is_MAR_S)) +
  geom_area() +
  theme_minimal()


(p1 + theme(legend.position = "None") | p2) / (p3 | p4)


# Check more recent effort ------------------------------------------------

effort_recent <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_FAO34_MONTHLY_LOW_2020-2024.csv")
l <- clean_effort(effort_recent, eez_overlap_sf, min_dist = 5, min_hours = 2000)
effort_recent <- l[[3]]

# , " - ", gear_type

effort_recent |> 
  mutate(flotilla = paste0(flag_iso3c)) |> 
  summarize(
    kWh = sum(kWh, na.rm = TRUE),
    .by = flotilla
  ) |> 
  arrange(desc(kWh)) |> 
  mutate(
    cumulated_percent = 100 * cumsum(kWh) / sum(kWh)
  ) |> 
  filter(cumulated_percent <= 90) |> 
  ggplot(aes(x = fct_reorder(flotilla, kWh), y = kWh)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "") +
  theme_minimal()

effort_recent |> 
  mutate(is_MAR_S = (flag_iso3c == "MAR") & (gear_type %in% c("Tuna purse seines", "Other seines"))) |> 
  summarize(
    kWh = sum(kWh, na.rm = TRUE),
    .by = c(year, is_MAR_S)
  ) |> 
  ggplot(aes(x = year, y = kWh, fill = is_MAR_S)) +
  geom_area() +
  theme_minimal()
