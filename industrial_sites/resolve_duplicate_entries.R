# ---------------------------------------------------------------------------
# R Script: Resolve Duplicate Entries
# ---------------------------------------------------------------------------
#
# Purpose:
# This script processes the duplicate results output from 
# `update_facility_coordinates_from_csv.R` and appends the resolved records. 
# These records must be resolved manually. A "Keep" column must exist in the provided
# .xlsx file. 
#
# The outputs of this script make the v5.1 of the facilities list
#
# Resolution Rules:
# 1. For duplicate facilities with multiple NAICS codes, retain all records 
#    but append a letter suffix to each facility ID (e.g., E110000323070-a, E110000323070-b).
# 2. For duplicates with the same NAICS code, keep one record. If one record has 
#    better address data and another has better employee counts, they can be manually merged.
#
# User Inputs:
# - `inwu_model_folder`: Path to the Industrial model folder.
# - `output_directory`: Directory for saving output files.
#
# Data Sources:
# 1. Duplicate Facilities List:
#    - File: `NOTES_duplicate_facility_ids_v5_20241114.xlsx`
#    - Location: 
#      `~\GS-W-WaterUse - Industrial model\INWU_task_folders\`
#      `Site_selection\Industrial_site_list\`
# 2. V5 Facilities List:
#    - File: `NOTES_duplicate_facility_ids_v5_20241114.xlsx`
#    - Location: 
#      `~\GS-W-WaterUse - Industrial model\INWU_task_folders\`
#      `Site_selection\Industrial_site_list\`
# ---------------------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(readxl)

### Getting User Inputs ###
inwu_model_folder <- "D:/DOI/GS-W-WaterUse - Industrial model"
output_directory <- 'outputs/update_facility_coordinates_from_csv'

### Create Output Directory if it doesn't exist ###
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)  # Create directory and any necessary parent directories
  cat("Directory created:", output_directory, "\n")
} 

### Read Duplicate Industrial Facilities List ### 
duplicate_facility_xlsx <- paste0(inwu_model_folder,
                                  "/INWU_task_folders/Site_selection/Industrial_site_list/NOTES_duplicate_facility_ids_v5_20241114.xlsx")

# Read the duplicate facilities data into a dataframe
duplicate_facility_df <- read_excel(duplicate_facility_xlsx)

### Read v4 of the Industrial Facilities List ### 
inwu_facility_csv <- paste0(inwu_model_folder,
                            "/INWU_task_folders/Site_selection/Industrial_site_list/USEPA_HIFLD_EIA_PPP_facility_v5.csv")

# Read the Industrial Facilities List into a dataframe with specified column types
inwu_facility_df <- read_csv(inwu_facility_csv,
                             col_types = cols(
                               LATITUDE = col_double(),
                               LONGITUDE = col_double(),  # Corrected typo: LONGTITUDE -> LONGITUDE
                               NAICS = col_character(),
                               EMP = col_integer(),
                               TotPerson = col_integer(),
                               JobsReported = col_integer()
                             ))

### Filter out facilities listed in the duplicate facilities list ###

# Get unique facility IDs without suffixes
duplicate_facility_ids <- unique(sub("-[a-z]$", "", duplicate_facility_df$FACILITYID))

# Exclude duplicate facilities from the main facilities dataframe
inwu_facility_df <- inwu_facility_df %>%
  filter(!FACILITYID %in% duplicate_facility_ids)

### Append retained duplicate facilities to the main dataframe ###

# Filter duplicate facilities list to include only records marked for keeping
fields_list <- names(duplicate_facility_df)
fields_list <- fields_list[!fields_list %in% c("Notes", "Keep")]

# Keep only the records marked with Keep == 1 and select relevant fields
duplicate_facility_df <- duplicate_facility_df %>%
  filter(Keep == 1) %>%
  select(fields_list)

## Combine the main facilities dataframe with the kept duplicate facilities
inwu_facility_df <- rbind(inwu_facility_df, duplicate_facility_df)

# Output the results to a CSV file
output_path <- paste0(output_directory, '/USEPA_HIFLD_EIA_PPP_facility_v5.1.csv')

# Write the updated facilities dataframe to CSV
write.csv(inwu_facility_df, output_path, row.names = FALSE)

