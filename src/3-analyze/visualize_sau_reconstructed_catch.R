library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(broom) 
library(ggplot2)

reconstructed_catch <- read_csv("output/data/sau_reconstructed_catch_clean.csv")

# Data is transformed as follows:
# - Only 'industrial' fishing is considered (see cleaning steps)
# - A flag is kept if:
#   - There are 2 years or more of data
#   - Its contribution to the total historical catch is more than 1 percent
#   - ...
# For visualization, only the 10 most fished species are shown. The rest is aggregated in 'Other'

get_catch_eez <- function(country = "Mauritania", 
                          catch = reconstructed_catch,
                          fig_save_path = "output/figures/sau_catch/"){
  
  catch_sample <- catch |> 
    filter(eez == country)
  
  last_year <- max(catch_sample$year)
  
  top_flag <- catch_sample |> 
    summarize(total_catch = sum(reconstructed_catch_tonnes, na.rm = TRUE),
              n_years = n_distinct(year),
              .by = flag) |> 
    mutate(contrib = 100 * total_catch / sum(total_catch, na.rm = TRUE)) |> 
    arrange(desc(contrib)) |> 
    mutate(cum_contrib = cumsum(contrib)) |> 
    filter(
      n_years > 1, 
      contrib > 1e-2, 
      cum_contrib <= 100
    )
  
  flag_levels <- top_flag |> select(flag) |>  pull()
  
  top10_species <- catch_sample |> 
    summarize(total_catch = sum(reconstructed_catch_tonnes, na.rm = TRUE), 
              .by = scientific_name) |> 
    arrange(desc(total_catch)) |> 
    head(10)
  
  taxa_levels <- append(top10_species |> select(scientific_name) |> pull(), 
                        "Other",
                        after = 0)
  
  catch_sample <- catch_sample |> 
    mutate(
      taxon = case_when(
        scientific_name %in% top10_species$scientific_name ~ scientific_name,
        TRUE ~ "Other"
      )
    ) |> 
    summarize(reconstructed_catch_tonnes = sum(reconstructed_catch_tonnes, na.rm = TRUE),
              .by = c(year, taxon, flag)) |> 
    filter(flag %in% top_flag$flag) |> 
    mutate(
      taxon = factor(taxon, levels = rev(taxa_levels)),
      flag = factor(flag, levels = flag_levels)
    )
  
  p <- catch_sample |> 
    summarize(reconstructed_catch_tonnes = sum(reconstructed_catch_tonnes, na.rm = TRUE),
              .by = c(year, taxon)) |> 
    ggplot(aes(x = year, y = reconstructed_catch_tonnes, fill = taxon)) +
    geom_area() +
    geom_vline(xintercept = last_year, color = "black") +
    labs(
      x = "Year",
      y = "Reconstructed catch (t/yr-1)",
      fill = "Taxon",
      title = paste0("Annual reconstructed catch from industrial fishing in ", country),
      subtitle = paste0("Catch from flagstates contributing less than 1% to the total is discarded.",
                        paste0("\nMost recent data: ", last_year)),
      caption = "Data from the Sea Around Us project - seaaroundus.org"
    ) +
    scale_fill_brewer(palette = "Set3") +
    xlim(1950, 2030) +
    theme_minimal()

  print(p)
  
  ggsave(paste0(fig_save_path, "sau_catch_total_", country, ".png"),
         p,
         height = 15,
         width = 25,
         units = "cm")
  
  p <- ggplot(catch_sample, aes(x = year, y = reconstructed_catch_tonnes, fill = taxon)) +
    geom_area() +
    geom_vline(xintercept = last_year, color = "black") +
    labs(
      x = "Year",
      y = "Reconstructed catch (t/yr-1)",
      fill = "Taxon",
      title = paste0("Annual reconstructed catch from industrial fishing in ", country, " per flagstate"),
      subtitle = paste0("Flagstates contributing less than 1% to the total catch are discarded.",
                        paste0("\nMost recent data: ", last_year)),
      caption = "Data from the Sea Around Us project - seaaroundus.org"
    ) +
    scale_fill_brewer(palette = "Set3") +
    xlim(1950, 2030) +
    theme_minimal() +
    facet_wrap(
      ~ flag,
      scales = "free"
    )
  
  ggsave(paste0(fig_save_path, "sau_catch_total_per_flag_", country, ".png"),
         p,
         height = 20,
         width = 40,
         units = "cm")
  
  return(catch_sample)
  
}

catch_sample_MRT <- get_catch_eez(country = "Senegal")
catch_sample_SEN <- get_catch_eez(country = "Mauritania")
catch_sample_GMB <- get_catch_eez(country = "Gambia")
catch_sample_GNB <- get_catch_eez(country = "Guinea-Bissau")

