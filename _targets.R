# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
packages <- c("tibble", "stringr", "purrr", "readxl", "svDialogs", "dplyr", "archive",
              "tidyr", "readr", "lubridate", "magrittr",
              "sf", "rquery", "officer", "pdftools","rqdatatable",
              "fedmatch", "janitor")
optinstall <- function(x) {
  if(!x %in% utils::installed.packages()) {
    install.packages(x)
  }
}
suppressWarnings(invisible(lapply(packages, optinstall)))

# Set target options:
tar_option_set(
  packages = packages, # Packages that your targets need for their tasks.
  format = "rds" # Optionally set the default storage format. qs is fast.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R")

# Replace the target list below with your own:
list(
  tar_target(datafp, "state_data", format = "file"),
  tar_target(existingCrosswalks, "DataCrosswalks", format = "file"),
  tar_target(dat, command = get_all_dat(datafp)),
  tar_target(updatedCrosswalks, command = updateCrosswalks(data = dat, existingCrosswalks = existingCrosswalks)),
  tar_target(renamed_rawdat, command = 
               readandrename_columns(datafp, updatedCrosswalks, existingCrosswalks)
             ),
  tar_target(reformatted_data, command = 
               reformat_data(renamed_rawdat, updatedCrosswalks, existingCrosswalks)
             ),
  tar_target(AllStates, command = write_allstates(reformatted_data), format = "file")
)

# For debugging ----
# lapply(packages, library, character.only = TRUE)
# tar_source(files = "R")
# tar_source(files = "utility_functions")
# tar_load_everything()
# tar_make(callr_function = NULL)