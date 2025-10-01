library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(ggplot2)

# Store final models ------------------------------------------------------

fit_data <- list()
final_models <- list()


# Functions ---------------------------------------------------------------

# Function to check model validity

check_model <- function(model, 
                        data,
                        norm_pvalue_thresh = 0.05,
                        var_pvalue_thresh = 0.05,
                        outliers_cook_thresh = 4) {
  diagnostics <- list()
  
  if (all(residuals(model) == 0)) {
    diagnostics$perfect_fit <- TRUE
    return(diagnostics)
  }
  
  diagnostics$perfect_fit <- FALSE
  
  # Check p-value
  diagnostics$p_value <- glance(model)$p.value
  
  # Check residual normality
  diagnostics$normality <- shapiro.test(residuals(model))$p.value > norm_pvalue_thresh
  
  # Check heteroscedasticity
  diagnostics$heteroscedasticity <- ncvTest(model)$p > var_pvalue_thresh
  
  # Check outliers using Cook's distance
  cooks_d <- cooks.distance(model)
  diagnostics$outliers <- max(cooks_d, na.rm = TRUE) < (outliers_cook_thresh / nrow(data))
  
  return(diagnostics)
}

# Function to fit and select the best model

generate_predictions <- function(sub_data, 
                                 verify_assumptions = TRUE,
                                 norm_pvalue_thresh = 0.05,
                                 var_pvalue_thresh = 0.05,
                                 outliers_cook_thresh = 4,
                                 prediction_period = 2020:2024) {
  
  if (nrow(sub_data) < 3) {
    return(data.frame(year = prediction_period, tonnes = NA, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "Not enough data"))
  }
  
  best_data <- NULL
  best_model <- NULL
  best_diagnostics <- NULL
  
  # Iterate over different data subsets to find the best model
  for (n in 3:nrow(sub_data)) {
    model <- lm(tonnes ~ year, data = sub_data[1:n, ])
    diagnostics <- check_model(model,
                               sub_data[1:n, ],
                               norm_pvalue_thresh,
                               var_pvalue_thresh,
                               outliers_cook_thresh)
    
    # If perfect fit, return immediately
    if (diagnostics$perfect_fit) {
      last_value <- head(sub_data$tonnes, 1)
      return(data.frame(year = prediction_period, tonnes = last_value, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "Perfect fit (prob. const.)"))
    }
    
    # If verify_assumptions is TRUE, reject models that fail any assumption
    if (verify_assumptions && (!diagnostics$normality || !diagnostics$heteroscedasticity || !diagnostics$outliers)) {
      next  # Skip this model and continue searching
    }
    
    # Update best model based on p-value
    if (is.null(best_model) || (diagnostics$p_value < best_diagnostics$p_value)) {
      best_data <- sub_data[1:n, ]
      best_model <- model
      best_diagnostics <- diagnostics
    }
  }
  
  # If no valid model is found, return NA
  if (is.null(best_model)) {
    return(data.frame(year = prediction_period, tonnes = NA, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "No valid model"))
  }
  
  # Store final model
  model_key <- paste(unique(sub_data$eez), unique(sub_data$scientific_name), sep = "_")
  fit_data[[model_key]] <<- best_data
  final_models[[model_key]] <<- best_model
  
  # Generate predictions
  new_data <- data.frame(year = prediction_period)
  preds <- predict(best_model, newdata = new_data, interval = "prediction")
  
  return(data.frame(year = prediction_period, tonnes = preds[,1], lwr = preds[,2], upr = preds[,3], pvalue = best_diagnostics$p_value, rsquared = glance(model)$r.squared, note = "Good fit"))
}


generate_predictions_mae <- function(sub_data, 
                                 verify_assumptions = TRUE,
                                 norm_pvalue_thresh = 0.05,
                                 var_pvalue_thresh = 0.05,
                                 outliers_cook_thresh = 4,
                                 prediction_period = 2020:2024) {
  
  if (nrow(sub_data) < 3) {
    return(data.frame(year = prediction_period, tonnes = NA, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "Not enough data"))
  }
  
  best_data <- NULL
  best_model <- NULL
  best_diagnostics <- NULL
  best_mae <- Inf  # Initialize with a large value
  
  # Iterate over different data subsets to find the best model
  for (n in 3:nrow(sub_data)) {
    model <- lm(tonnes ~ year, data = sub_data[1:n, ])
    diagnostics <- check_model(model,
                               sub_data[1:n, ],
                               norm_pvalue_thresh,
                               var_pvalue_thresh,
                               outliers_cook_thresh)
    
    # If perfect fit, return immediately
    if (diagnostics$perfect_fit) {
      last_value <- head(sub_data$tonnes, 1)
      return(data.frame(year = prediction_period, tonnes = last_value, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "Perfect fit (prob. const.)"))
    }
    
    # If verify_assumptions is TRUE, reject models that fail any assumption
    if (verify_assumptions && (!diagnostics$normality || !diagnostics$heteroscedasticity || !diagnostics$outliers)) {
      next  # Skip this model and continue searching
    }
    
    # Calculate Mean Absolute Error (MAE) as selection criterion
    residuals_abs <- abs(residuals(model))
    mae <- mean(residuals_abs)
    
    # Update best model based on lowest MAE
    if (mae < best_mae) {
      best_mae <- mae
      best_data <- sub_data[1:n, ]
      best_model <- model
      best_diagnostics <- diagnostics
    }
  }
  
  # If no valid model is found, return NA
  if (is.null(best_model)) {
    return(data.frame(year = prediction_period, tonnes = NA, lwr = NA, upr = NA, pvalue = NA, rsquared = NA, note = "No valid model"))
  }
  
  # Store final model
  model_key <- paste(unique(sub_data$eez), unique(sub_data$scientific_name), sep = "_")
  fit_data[[model_key]] <<- best_data
  final_models[[model_key]] <<- best_model
  
  # Generate predictions
  new_data <- data.frame(year = prediction_period)
  preds <- predict(best_model, newdata = new_data, interval = "prediction")
  
  return(data.frame(year = prediction_period, tonnes = preds[,1], lwr = preds[,2], upr = preds[,3], pvalue = best_diagnostics$p_value, rsquared = glance(best_model)$r.squared, note = "Good fit"))
}



# Function to visualize the results for one taxon and one EEZ

plot_species <- function(species_name = "Ablennes hians",
                         eez_name = "Mauritania",
                         results = data.frame(),
                         final_models = list(),
                         fit_data = list()) {
  fit_type <- results |> 
    filter(scientific_name == species_name, eez == eez_name) |> 
    distinct(note) |> 
    pull()
  
  species_ts <- data |>
    filter(scientific_name == species_name, eez == eez_name)
  
  model_key <- paste(eez_name, species_name, sep = "_")
  model <- final_models[[model_key]]
  
  infos <- glance(model)
  
  sau_data <- data |> filter(eez == eez_name, scientific_name == species_name)
  
  if (!is.null(model)) {
    
    predicted_data <- results |> filter(eez == eez_name, scientific_name == species_name)
    
    model_fit_data <- fit_data[[model_key]]
    
    regline_data <- data.frame(year = min(model_fit_data$year):2024)
    preds <- predict(model, newdata = regline_data, interval = "prediction")
    regline_data <- data.frame(year = min(model_fit_data$year):2024, tonnes = preds[,1], lwr = preds[,2], upr = preds[,3], note = fit_type)
    
    p <- ggplot() +
      geom_line(data = regline_data, aes(x = year, y = tonnes), color = "red") +
      geom_ribbon(data = regline_data, aes(x = year, ymin = lwr, ymax = upr), fill = "red", alpha = 0.5) +
      geom_line(data = sau_data, aes(x = year, y = tonnes), color = "black") +
      geom_point(data = model_fit_data, aes(x = year, y = tonnes), color = "blue") +
      geom_point(data = predicted_data, aes(x = year, y = tonnes), color = "black") +
      geom_errorbar(data = predicted_data, aes(x = year, ymin = lwr, ymax = upr), color = "black") +
      scale_x_continuous(
        breaks = seq(1950, 2025, 5),
        limits = c(1950, 2025)
      ) +
      labs(
        x = "Year", y = "Annual catch (tonnes of wet weight)",
        title = paste0("Catch of ", species_name, " in ", eez_name),
        subtitle = paste(paste0("LM fit type: ", fit_type),
                         paste0("R² = ", round(infos$r.squared, 2)),
                         paste0("p-value = ", round(infos$p.value, 7)),
                         sep = "\n")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45))
    
    return(p)
  }
  
  p <- ggplot() +
    geom_line(data = sau_data, aes(x = year, y = tonnes), color = "black") +
    scale_x_continuous(
      breaks = seq(1950, 2025, 5),
      limits = c(1950, 2025)
    ) +
    labs(
      x = "Year", y = "Annual catch (tonnes of wet weight)",
      title = paste0("Catch of ", species_name, " in ", eez_name),
      subtitle = paste(paste0("LM fit type: ", fit_type))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))
  
  if (fit_type == "Perfect fit (prob. const.)") {
    
    value <- results |> 
      filter(scientific_name == species_name, eez == eez_name) |> 
      distinct(tonnes) |> 
      pull()
    
    p <- p +
      geom_segment(aes(x = 2020, xend = 2024,
                       y = value, yend = value), color = "red")
  }
  
  return (p)
}

# Function to plot and save all the time series and their possible extrapolation

plot_results <- function(results,
                         final_models = list(),
                         fit_data = list(),
                         save_path = "output/figures/reconstructed_catch/species_time_series_extrapolated/automatic/") {
  
  simple_types <- list()
  
  simple_types[["Good fit (95% CI lower bound > 0)"]] <- "good_pos"
  simple_types[["Good fit (negative predictions)"]] <- "good_neg"
  simple_types[["Good fit (95% CI lower bound <= 0)"]] <- "good_CI_lwr_neg"
  simple_types[["Perfect fit (prob. const.)"]] <- "perfect_const"
  simple_types[["No valid model"]] <- "no_valid_model"
  simple_types[["Not enough data"]] <- "not_enough_data"
  simple_types[["LessHyp: Good fit (sign. > 0)"]] <- "good_pos"
  simple_types[["LessHyp: Good fit (0)"]] <- "good_zero"
  simple_types[["LessHyp: Still no valid model"]] <- "no_valid_model"
  
  for (eez_name in distinct(results, eez) |>  pull()) {
    for (name in distinct(results, scientific_name) |> pull()) {
      
      info <- results |> 
        filter(scientific_name == name, eez == eez_name)
      
      if (nrow(info) == 0) {
        next
      }
      
      fit_type <- info |> 
        distinct(note) |> 
        pull()
      
      filename <- paste0(save_path, eez_name,"/", simple_types[[fit_type]], "/", name, "_", eez_name, "_", "extrapolated.png")
      
      p <- plot_species(name, eez_name, results, final_models, fit_data)
      ggsave(filename = filename,
             plot = p,
             width = 22,
             height = 15,
             units = "cm",
             create.dir = TRUE)
    }
  }
}