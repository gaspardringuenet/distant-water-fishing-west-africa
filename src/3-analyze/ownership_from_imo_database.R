library(openxlsx)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(patchwork)

# Load data completed via the GISIS database ------------------------------

data <- read.xlsx(xlsxFile = "output/data/ownership/vessel_ranking_for_ownership_v2.xlsx",
                  sheet = "IMO_list")

effort_without_imo <- data |> filter(is.na(imo)) |> pull(apparent_fishing_hours)
total_effort <- sum(data$apparent_fishing_hours)

print(paste("We obtained IMO numbers for", round(total_effort - effort_without_imo), "hours of apparent fishing effort."))
print(paste("This represents", round(100* (total_effort - effort_without_imo) / total_effort, 1), "% of the total effort in MRT, SEN, GMB and GNB between 2020 and 2024."))

data <- data |> 
  filter(!is.na(imo))

data <- data |> 
  mutate(effort_percent = 100 * apparent_fishing_hours / total_effort)

# Visualization -----------------------------------------------------------

countries <- sort(unique(c(data$company_nationality, data$company_address_country)))
color_palette <- setNames(colorRampPalette(brewer.pal(9, "Set1"))(length(countries)), countries)

p1 <- data |> 
  summarize(effort_percent = sum(effort_percent, na.rm = TRUE),
            .by = c(registered_owner, company_nationality)) |> 
  head(30) |> 
  ggplot(aes(x = fct_reorder(registered_owner, effort_percent), y = effort_percent, fill = company_nationality)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(y = "% of total effort", x = "",
       title = "Effort by company - top 30",
       fill = "Company nationality") 

p2 <- data |> 
  summarize(effort_percent = sum(effort_percent, na.rm = TRUE),
            .by = c(registered_owner, company_address_country)) |> 
  head(30) |> 
  ggplot(aes(x = fct_reorder(registered_owner, effort_percent), y = effort_percent, fill = company_address_country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(y = "% of total effort", x = "",
       title = "Effort by company - top 30",
       fill = "Company address") 

p3 <- data |> 
  summarize(effort_percent = sum(effort_percent, na.rm = TRUE),
            .by = c(company_nationality)) |> 
  ggplot(aes(x = fct_reorder(company_nationality, effort_percent), y = effort_percent, fill = company_nationality)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(y = "% of total effort", x = "",
       title = "Apparent effort by registered owner nationality") +
  theme(legend.position = "None")

p4 <- data |> 
  summarize(effort_percent = sum(effort_percent, na.rm = TRUE),
            .by = c(company_address_country)) |> 
  ggplot(aes(x = fct_reorder(company_address_country, effort_percent), y = effort_percent, fill = company_address_country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_palette) +
  coord_flip() +
  labs(y = "% of total effort", x = "",
       title = "Apparent effort by registered owner address") +
  theme(legend.position = "None")


(p1 + p2) / (p3 + p4)
