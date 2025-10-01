library(readr)
library(openxlsx)
library(dplyr)
library(ggplot2)

# Load fishing effort data ------------------------------------------------

data <- read_csv("output/data/gfw_apparent_fishing_effort/gfw_effort_FAO34_MONTHLY_LOW_2020-2024.csv")


# Format and compute cumulated sum ----------------------------------------

vessels <- data |> 
  summarize(apparent_fishing_hours = sum(apparent_fishing_hours, na.rm = TRUE),
            .by = c(flag, vessel_id, mmsi, vessel_name, imo, call_sign)) |> 
  arrange(desc(apparent_fishing_hours)) |> 
  mutate(effort_cumulated = cumsum(apparent_fishing_hours),
         effort_cumulated_percent = 100 * effort_cumulated / sum(apparent_fishing_hours)) |> 
  select(-effort_cumulated)

vessels$rank <- as.numeric(rownames(vessels))


# Summarize the data ------------------------------------------------------

vessels |> select(imo) |>  distinct() |>  nrow()
  
ggplot(vessels, aes(x = rank, y = effort_cumulated_percent)) +
  geom_line() +
  labs(x = "Rank", y = "Cumulated effort (%)") +
  theme_minimal() 
ggsave("output/figures/gfw_apparent_fishing_effort_FAO34_2020-2024/cumulated_effort_vs_rank.png",
       height = 10,
       width = 15,
       units = "cm")

# Save --------------------------------------------------------------------

write.xlsx(vessels,
           "output/data/ownership/vessel_ranking_for_ownership_FAO34.xlsx")



