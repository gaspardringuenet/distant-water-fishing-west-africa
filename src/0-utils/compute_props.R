library(dplyr)

compute_prop_in_eez <- function(eez_code, flag_code, var = "kWh") {
  # For a given set of EEZs, compute the contribution of given flags the total effort 
  var_sym <- sym(var)
  
  if ((length(flag_code) == 1) && (flag_code == "ALL")) {
    flag_code <- unique(effort$flag_iso3c)
  }
  if ((length(eez_code) == 1) && (eez_code == "ALL")) {
    eez_code <- unique(effort$eez_iso3c)
  }
  
  effort |> 
    summarize(
      value = sum(!!var_sym, na.rm = TRUE),
      .by = c(flag_iso3c, eez_iso3c)
    ) |> 
    filter(eez_iso3c %in% c(eez_code)) |> 
    mutate(percent = 100 * value / sum(value, na.rm = TRUE)) |> 
    filter(flag_iso3c %in% c(flag_code)) |> 
    pull(percent) |> 
    sum()
}

compute_prop_by_flag <- function(eez_code, flag_code, var = "kWh") {
  # For a given set of flags, compute the proportion of their effort that occured in given EEZs
  var_sym <- sym(var)
  
  if ((length(flag_code) == 1) && (flag_code == "ALL")) {
    flag_code <- unique(effort$flag_iso3c)
  }
  if ((length(eez_code) == 1) && (eez_code == "ALL")) {
    eez_code <- unique(effort$eez_iso3c)
  }
  
  effort |> 
    summarize(
      value = sum(!!var_sym, na.rm = TRUE),
      .by = c(flag_iso3c, eez_iso3c)
    ) |> 
    filter(flag_iso3c %in% c(flag_code)) |> 
    mutate(percent = 100 * value / sum(value, na.rm = TRUE)) |> 
    filter(eez_iso3c %in% c(eez_code)) |> 
    pull(percent) |> 
    sum()
}
