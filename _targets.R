# Title: Targets Pipeline Script
# Description: This script defines a data processing pipeline using the targets package.
# Author: Cathy Chamberlin (cchamberlin@usgs.gov) & Lily Gorman Sanisaca (lgormansanisaca@usgs.gov)
# Date: 8/22/24
#
# Follow this script to set up your data processing pipeline using the targets library.

# Load necessary packages
library(targets)

# Specify the packages required for the pipeline
packages <- c("tibble", "stringr", "purrr", "readxl", "svDialogs", "dplyr", "archive",
              "tidyr", "readr", "lubridate", "magrittr",
              "sf", "rquery", "officer", "pdftools", "rqdatatable",
              "fedmatch", "janitor", "zoo", "varhandle")

# Function to install missing packages
optinstall <- function(x) {
  if (!x %in% utils::installed.packages()) {
    install.packages(x)  # Install package if it's not already installed
  }
}

# Suppress warnings and install any missing packages
suppressWarnings(invisible(lapply(packages, optinstall)))

# Set options for the targets package
tar_option_set(packages = packages,  # Define required packages
               format = "rds")        # Set default storage format to "rds"

# Source custom R scripts from the "R" directory
tar_source(files = "R")

# Define the list of targets for the pipeline
list(
  tar_target(datafp, "state_data", format = "file"),  # Load state data file
  tar_target(existingCrosswalks, "DataCrosswalks/StateDataCrosswalks", format = "file"),  # Load existing state crosswalks
  tar_target(NationalDataCrosswalks, "DataCrosswalks/NationalDataCrosswalks", format = "file"), # Load national crosswalks
  tar_target(NAICSworkup, "Industrial model/Industrial_DataSummary_By_LEE.xlsx", format = "file"), # Load NAICS summary
  tar_target(SWUDS, "Industrial model/SWUDS_records/natprod_CN_QNTY_industrial_2024-07-12.xlsx", format = "file"), # Load SWUDS records
  tar_target(dat, command = get_all_dat(datafp)),  # List all data from the state data file
  tar_target(updatedCrosswalks, command = updateCrosswalks(data = dat, existingCrosswalks = existingCrosswalks)),  # Update state crosswalks with new data
  tar_target(renamed_rawdat, command = 
               readandrename_columns(datafp, updatedCrosswalks, existingCrosswalks, data = "State") # Read and rename columns in the data
  ),
  tar_target(reformatted_data, command = 
               reformat_data(renamed_rawdat, updatedCrosswalks, existingCrosswalks, data = "State") # Reformat the data based on crosswalks
  ),
  tar_target(combined_dat, command = 
               merge_nationaldata(nonSWUDS = reformatted_data, 
                                  national_Xwalks = NationalDataCrosswalks, 
                                  datacodes_Xwalks = updatedCrosswalks$DataCodesCrosswalk,
                                  natdata = list(NAICSworkup, SWUDS))), # Combine state data with national data
  tar_target(AllStates, command = write_allstates(combined_dat), format = "file")  # Write combined data for all states to a file
)

# For debugging ----
# lapply(packages, library, character.only = TRUE)  # Load all required packages 
# tar_source(files = "utility_functions")  # Source utility functions 
# tar_load_everything()  # Load all targets into the environment
# tar_make(callr_function = NULL)  # Run the pipeline in the current R session