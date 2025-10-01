library(readr)
library(openxlsx)
library(dplyr)
library(stringr)


# Load data completed via the GISIS database ------------------------------

previous <- read.xlsx(xlsxFile = "data/ownership/vessel_ranking_for_ownership_completed.xlsx",
                      sheet = "complete_imo")
new <- read.xlsx(xlsxFile = "data/ownership/vessel_ranking_for_ownership_FAO34.xlsx",
                 sheet = "complete_imo")

new_with_prev <- new |> 
  left_join(previous[, 6:ncol(previous)] |> 
              select(-c(mmsi, gfw_vessel_name, gfw_imo)),
            by = "vessel_id",
            relationship = "one-to-one")

wb <- loadWorkbook("data/ownership/vessel_ranking_for_ownership_FAO34.xlsx")
addWorksheet(wb,"complete_imo2")
writeData(wb, "complete_imo2", new_with_prev)
saveWorkbook(wb, "data/ownership/vessel_ranking_for_ownership_FAO34.xlsx", overwrite = TRUE)
