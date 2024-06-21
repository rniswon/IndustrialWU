# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
packages <- c("tibble", "stringr", "purrr", "readxl", "svDialogs", "dplyr", "archive",
              "furrr", "tidyr", "future", "readr", "lubridate", "tigris",
              "zipcodeR", "data.table", "sf", "rquery", "officer")

# Set target options:
tar_option_set(
  packages = packages, # Packages that your targets need for their tasks.
  format = "rds" # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R")

# Replace the target list below with your own:
list(
  tar_target(datafp, "C:/Users/cchamber/DOI/GS-W-WaterUse - Documents/GAP/industrial/state_data", format = "file"),
  tar_target(existingDataDictionary, "DataCrosswalks/DataDirectories.csv", format = "file"),
  tar_target(existingHeaderCrosswalk, "DataCrosswalks/HeaderCrosswalk.csv", format = "file"),
  tar_target(existingHardCodes, "DataCrosswalks/HardcodedManualAttributes.csv", format = "file"),
  tar_target(existingpivots, "DataCrosswalks/DataPivots.csv", format = "file"),
  tar_target(existingCodesCrosswalk, "DataCrosswalks/DataCodesCrosswalk.csv", format = "file"),
  tar_target(dat, command = get_all_dat(datafp)),
  tar_target(blankDataDictionary, command = generate_blankcsv(dat)),
  tar_target(updatedDataDictionary, command = merge_data(blank = blankDataDictionary, filled = existingDataDictionary)),
  tar_target(blankHeaderCrosswalk, command = generate_blankHeaderCrosswalkcsv(updatedDataDictionary)),
  tar_target(updatedHeaderCrosswalk, command = merge_data(blank = blankHeaderCrosswalk, filled = existingHeaderCrosswalk)),
  tar_target(renamed_rawdat, command = 
               readandrename_columns(datafp, updatedHeaderCrosswalk, existingpivots, existingHardCodes)
             ),
  tar_target(reformatted_data, command = 
               reformat_data(renamed_rawdat, updatedHeaderCrosswalk, existingHardCodes, existingCodesCrosswalk) 
             )
)

# For debugging ----
# lapply(packages, library, character.only = TRUE)
# tar_source(files = "R")
# tar_source(files = "utility_functions")
# tar_load_everything()