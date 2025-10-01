library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(patchwork)
library(ggplot2)

# Data --------------------------------------------------------------------

reconstructed_catch <- read_csv("output/data/sau_reconstructed_catch_clean.csv") |> 
  filter(eez != "Guinea-Bissau")

# Functions ---------------------------------------------------------------

source("src/0-utils/automatic_extrapolation_lm_utils.R")

HYPER_PARAMS <- data.frame(
  min_year = 2005,
  verify_assumptions = TRUE,
  optimisation_criterion = "Mean Absolute Error",
  norm_pvalue_thresh = 0.01,
  var_pvalue_thresh = 0.05,
  outliers_cook_thresh = 4,
  fig_save_path = "output/figures/sau_catch/extrapolation/automatic_v3_mae/",
  data_save_path = "output/data/sau_catch/extrapolation/automatic_v3_mae/"
)

dir.create(HYPER_PARAMS$fig_save_path, recursive = TRUE, showWarnings = FALSE)
dir.create(HYPER_PARAMS$data_save_path, recursive = TRUE, showWarnings = FALSE)

write.csv(HYPER_PARAMS,
          paste0(HYPER_PARAMS$data_save_path, "hyper_parameters.csv"))

# Extrapolate time series for MRT, SEN & GMB ------------------------------

data <- reconstructed_catch |> 
  summarize(tonnes = sum(reconstructed_catch_tonnes, na.rm = TRUE),
            .by = c(year, eez, scientific_name, common_name))

final_models <- list()
fit_data <- list()

results <- data |> 
  filter(year >= HYPER_PARAMS$min_year) |>  
  group_by(eez, scientific_name) |> 
  arrange(desc(year)) |> 
  group_modify(~ generate_predictions_mae(., 
                                      verify_assumptions = HYPER_PARAMS$verify_assumptions,
                                      norm_pvalue_thresh = HYPER_PARAMS$norm_pvalue_thresh,
                                      var_pvalue_thresh = HYPER_PARAMS$var_pvalue_thresh,
                                      outliers_cook_thresh = HYPER_PARAMS$outliers_cook_thresh,
                                      ), .keep = TRUE) |> 
  ungroup()

results <- results |> 
  group_by(eez, scientific_name) |> 
  mutate(
    note = case_when(
      note == "Good fit" & min(lwr) > 0 ~ "Good fit (95% CI lower bound > 0)",
      note == "Good fit" & min(tonnes) < 0 ~ "Good fit (negative predictions)",
      note == "Good fit" & min(lwr) <= 0 ~ "Good fit (95% CI lower bound <= 0)",
      TRUE ~ note
    )
  ) |> 
  ungroup()


# Summary: Quantify how much data we were able to extrapolate

summary_results <- data |>
  filter(year >= 2014 & year <= 2019) |> 
  left_join(results |>  distinct(eez, scientific_name, note), 
            by = join_by(eez, scientific_name)) |> 
  summarize(tonnes = sum(tonnes, na.rm = TRUE),
            n_taxa = n_distinct(scientific_name),
            .by = c(eez, note)) |>
  group_by(eez) |> 
  mutate(
    tonnes_prop = round(tonnes / sum(tonnes, na.rm = TRUE), 2),
    taxa_prop = round(n_taxa / sum(n_taxa, na.rm = TRUE), 2),
    ) |> 
  ungroup() |> 
  arrange(eez, note)

hyper_params_text <- paste(paste0("     - ", names(HYPER_PARAMS)[1:5]), HYPER_PARAMS[1, 1:5], sep = ": ", collapse = "\n")

summary_results |> 
  select(-c(tonnes_prop, taxa_prop)) |> 
  rename("Tonnes of wet weight" = "tonnes", "N. of taxa" = "n_taxa") |> 
  pivot_longer(
    cols = c("Tonnes of wet weight", "N. of taxa"),
    names_to = "variable",
    ) |> 
  ggplot(aes(x = eez, y = value, fill = note)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "EEZ", y = "",
       fill = "Type of LM fit",
       title = "Proportion of catch correctly extrapolated by linear models",
       subtitle = paste("Computed on reference data: SAU data 2014 to 2019",
                        "Hyper parameters:",
                        hyper_params_text,
                        sep = "\n")) +
  facet_wrap(~ variable)
ggsave(paste0(HYPER_PARAMS$fig_save_path, "summary.png"),
       height = 15,
       width = 30,
       units = "cm",
       create.dir = TRUE)

# Visualize results -------------------------------------------------------

PLOT <- TRUE

if (PLOT) {
  plot_results(results,
               final_models,
               fit_data,
               save_path = HYPER_PARAMS$fig_save_path)
}

# Save results and models -------------------------------------------------

write.csv(results, paste0(HYPER_PARAMS$data_save_path, "extrapolation_results.csv"),
          row.names = FALSE)

write.csv(summary_results, paste0(HYPER_PARAMS$data_save_path, "extrapolation_results_summary.csv"),
          row.names = FALSE)

saveRDS(final_models, paste0(HYPER_PARAMS$data_save_path, "linear_models.rds"))
