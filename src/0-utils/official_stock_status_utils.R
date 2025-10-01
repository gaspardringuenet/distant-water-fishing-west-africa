library(openxlsx)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)

cecaf <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "cecaf_clean")  |> clean_names()
iccat <- read.xlsx("data/iccat_cecaf_stock_evaluations.xlsx", sheet = "iccat_clean") |> clean_names()

match_cecaf_assessment <- function(sau_catch, cecaf){
  
  # Prepare catch data by separating the taxonomic levels
  
  catch <- catch |> 
    separate(
      scientific_name,
      into = c("catch_taxa_higher", "catch_taxa_lower"),
      sep = " ",
      remove = FALSE
    )

  # Match to CECAF assessments (although only valid for recent years)
  
  cecaf_mod <- cecaf |> 
    select(species, cecaf_subregion, assessment_fr, assessment_level) |> 
    separate(
      species,
      into = c("eval_taxa_higher", "eval_taxa_lower"),
      sep = " ",
      remove = FALSE
    ) |> 
    mutate(
      eval_lme = case_when(
        cecaf_subregion == "North-Western Africa" ~ "LME 27",
        cecaf_subregion == "South" ~ "LME 28"
      )
    ) |> 
    rename(
      cecaf_assessment_fr = assessment_fr, 
      cecaf_assessment_level = assessment_level
    )
  
  # Join by exact match
  exact_match <- catch |> 
    left_join(
      cecaf_mod,
      by = c("catch_taxa_higher" = "eval_taxa_higher", "catch_taxa_lower" = "eval_taxa_lower"),
      relationship = "many-to-many"                             # many-to-many bc. northern and southern stocks are present in evaluation
    ) |> 
    filter(area_name == eval_lme)                               # here we filter so that eventually we have a many-to-one
  
  # Join by higher-level match (catch at species, CECAF at genus level)
  higher_level_match <- catch |> 
    left_join(
      cecaf_mod |> filter(is.na(eval_taxa_lower) | eval_taxa_lower %in% c("sp.", "spp.")),
      by = c("catch_taxa_higher" = "eval_taxa_higher"),
      relationship = "many-to-many"                            
    ) |> 
    filter(area_name == eval_lme)
  
  # Combine both
  cecaf_match <- bind_rows(exact_match, higher_level_match)
  
  no_cecaf_match <- setdiff(
    catch,
    cecaf_match[, colnames(catch)]
  )
  
  catch <- bind_rows(cecaf_match, no_cecaf_match) |> 
    select(-c(species, eval_taxa_lower, cecaf_subregion, eval_lme, catch_taxa_higher, catch_taxa_lower))
  
  # Return result
  return(catch)
}


match_iccat_assessment <- function(sau_catch, iccat) {
  
  # Prepare catch data by separating the taxonomic levels
  
  catch <- catch |> 
    separate(
      scientific_name,
      into = c("catch_taxa_higher", "catch_taxa_lower"),
      sep = " ",
      remove = FALSE
    )
  
  # Match to ICCAT assessments (although only valid for recent year)
  
  iccat_mod <- iccat |> 
    select(species, overfishing, overfished) |> 
    separate(
      species,
      into = c("eval_taxa_higher", "eval_taxa_lower"),
      sep = " ",
      remove = FALSE
    )
  
  # Join by exact match
  catch <- catch |> 
    left_join(
      iccat_mod,
      by = c("catch_taxa_higher" = "eval_taxa_higher", "catch_taxa_lower" = "eval_taxa_lower"),
      relationship = "many-to-one"
    ) |> 
    select(-c(species, catch_taxa_higher, catch_taxa_lower))
  
  # No higher level match bc. all assessments are at the species level
  # /!\ Note: We cannot state the level of (over-)exploitation when the catch is less precise taxonomically than the assessments
  
  return(catch)

}