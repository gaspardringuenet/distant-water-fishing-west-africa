library(readr)
library(stringr)
library(dplyr)
library(janitor)

# Helper function ---------------------------------------------------------

fix_csv_trailing_missing_field <- function(input_csv, output_csv, expected_cols = 15) {
  lines <- read_lines(input_csv)
  
  # Store header and data separately
  header <- lines[1]
  data_lines <- lines[-1]
  
  # Define expected number of columns from header
  expected_cols <- str_count(header, ",") + 1
  
  fixed_data_lines <- vapply(data_lines, function(line) {
    # Split on commas outside quotes
    fields <- strsplit(line, ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", perl = TRUE)[[1]]
    
    if (length(fields) == expected_cols - 1) {
      # Insert empty field as last column (landed_value missing)
      fields <- c(fields, "")
    }
    
    paste(fields, collapse = ",")
  }, FUN.VALUE = character(1))
  
  # Write back with original header
  write_lines(c(header, fixed_data_lines), output_csv)
  
  message("CSV fixed and saved to: ", output_csv)
  
  invisible(TRUE)
}

# Fix missing comma issue -------------------------------------------------

files <- c(
  "data/catch/reconstructed_catch/SAU FAO 34 v50-1/SAU FAO 34 v50-1",
  "data/catch/reconstructed_catch/SAU LME 27 v50-1/SAU LME 27 v50-1",
  "data/catch/reconstructed_catch/SAU LME 28 v50-1/SAU LME 28 v50-1"
  )

sapply(files, 
       function(f) {
         fix_csv_trailing_missing_field(
           input_csv = paste0(f, ".csv"), 
           output_csv = paste0(f, "_fixed.csv")
         )
       }
)


# Load fixed data ---------------------------------------------------------

catch_tot <- read_csv("data/catch/reconstructed_catch/SAU FAO 34 v50-1/SAU FAO 34 v50-1_fixed.csv") |> clean_names()
catch_lme27 <- read_csv("data/catch/reconstructed_catch/SAU LME 27 v50-1/SAU LME 27 v50-1_fixed.csv") |> clean_names()
catch_lme28 <- read_csv("data/catch/reconstructed_catch/SAU LME 28 v50-1/SAU LME 28 v50-1_fixed.csv") |> clean_names()

catch_lme <- catch_lme27 |> 
  mutate(area_name = "LME 27") |> 
  rbind(catch_lme28 |> 
          mutate(area_name = "LME 28"))

cols <- colnames(catch_lme)
n <- length(cols)
cols <- cols[-c(1, n-1, n)]

catch_lme_tot <- catch_lme |> 
  summarize(
    tonnes = sum(tonnes, na.rm = TRUE),
    landed_value = sum(landed_value, na.rm = TRUE),
    .by = c(cols)
  )

cols <- cols[-1]

catch_high_seas <- catch_tot |>
  left_join(
    catch_lme_tot |> select(-area_type),
    by = cols
  ) |> 
  mutate(
    tonnes = pmax(tonnes.x - tonnes.y, 0),                      # In some cases the catch is higher in the LMEs than in the whole FAO 34.
    landed_value = pmax(landed_value.x - landed_value.y, 0)     # Prob. bc LME 28 isn't fully contained in FAO 34.
  ) |> 
  select(-c(tonnes.x, tonnes.y, landed_value.x, landed_value.y)) |> 
  mutate(area_name = "FAO 34 - {LMEs}")

catch <- rbind(
  catch_lme,
  catch_high_seas
)

write.csv(catch,
          "output/data/sau_catch/sau_catch_fao34_HS_LMEs_clean.csv",
          row.names = FALSE)
