library(dplyr)
library(zoo)
library(purrr)

# Check for at least `n` consecutive non-zero values
has_consecutive_years <- function(x, n = 5) {
  rle_nonzero <- rle(x > 0)
  any(rle_nonzero$values & rle_nonzero$lengths >= n)
}

# Assign stock status if valid
classify_sau_stock_status <- function(df) {
  df <- df %>% arrange(year)
  df <- df %>% mutate(tonnes_ma = rollmean(tonnes, 3, fill = NA, align = "center"))
  
  peak_catch <- max(df$tonnes_ma, na.rm = TRUE)
  peak_year <- df$year[which.max(df$tonnes_ma)]
  post_peak <- df %>% filter(year > peak_year)
  
  if (nrow(post_peak) == 0) {
    df$status <- "Not assessed"
    return(df)
  }
  
  post_peak_min_catch <- min(post_peak$tonnes_ma, na.rm = TRUE)
  post_peak_min_year <- post_peak$year[which.min(post_peak$tonnes_ma)]
  
  df <- df %>%
    mutate(status = case_when(
      (tonnes_ma <= 0.1 * peak_catch & year > peak_year) ~ "Collapsed",
      (tonnes_ma > 0.1 * peak_catch & tonnes_ma < 0.5 * peak_catch & year > post_peak_min_year) ~ "Rebuilding",
      (tonnes_ma < 0.5 * peak_catch & tonnes_ma >= 0.1 * peak_catch & year > peak_year) ~ "Over-exploited",
      (tonnes_ma >= 0.5 * peak_catch) ~ "Exploited",
      (tonnes_ma <= 0.5 * peak_catch & (year < peak_year | year == peak_year & year == max(df$year))) ~ "Developing",
      TRUE ~ NA_character_
    ))
  
  return(df)
}

# # Apply to all stocks, labeling unqualified ones
# classified_data <- catch_timeseries %>%
#   group_by(scientific_name) %>%
#   group_split() %>%
#   map_dfr(function(stock_df) {
#     stock_df <- stock_df %>% arrange(year)
#     span_years <- max(stock_df$year) - min(stock_df$year)
#     total_catch <- sum(stock_df$tonnes, na.rm = TRUE)
#     has_5_consecutive <- has_consecutive_years(stock_df$tonnes, 5)
#     
#     if (span_years >= 10 && total_catch >= 1000 && has_5_consecutive) {
#       classify_stock_status(stock_df)
#     } else {
#       stock_df$status <- "Not assessed"
#       stock_df
#     }
#   })
