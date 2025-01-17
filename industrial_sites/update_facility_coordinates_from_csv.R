# ---------------------------------------------------------------------------
# R Script: Update Facility Coordinates
# ---------------------------------------------------------------------------
#
# Purpose:
# This script updates the geographical coordinates (latitude and longitude) 
# of industrial facilities based on outputs from Judy's  
# `Geo_code_Add_lat_Long.R`Script. It reads a 
# list of facilities, merges it with geocoded data, and updates the 
# coordinates where applicable.
#
# Dependencies:
# - utility_functions/update_facility_coordinates.R: Contains the function 
#   `update_facility_coordinates` used to update facility coordinates.
#
# User Inputs:
# - inwu_model_folder: Path to the Industrial model folder.
# - output_directory: Directory for output files
#
# Data Sources:
# 1. Industrial Facilities List (v4):
#    - File: USEPA_HIFLD_EIA_PPP_facility_v4.csv
#    - Location: D:/DOI/GS-W-WaterUse - Industrial model/INWU_task_folders/
#      Site_selection/Industrial_site_list/
# 2. Geocoding Outputs:
#    - Location: ~/GS-W-WaterUse - Industrial model/INWU_task_folders/
#      Site_selection/Data/_CLEANUP/Find_Lats_Longs/NAICS_Address
#    - Files are the outputs of `Geo_code_Add_lat_Long.R`
#    - Files are named in the format: NAICS_[3-digit Code]_[Category Name]_
#      [Source]_geo.csv
#
# Processing Steps:
# 1. Read the Industrial Facilities List from CSV.
# 2. List CSV files from geocoding output directory.
# 3. Update facility coordinates by merging with geocoded data from each CSV.
# 4. Count and print the number of updated records.
#
# Output:
# - Prints the number of coordinates that were updated to the console.
# - Writes the updated facilities dataframe and a list of duplicates to CSV files.

library(tidyverse)
source("utility_functions/update_facility_coordinates.R")

### Getting User Inputs ###
inwu_model_folder <- "D:/DOI/GS-W-WaterUse - Industrial model"
output_directory <- 'outputs/update_facility_coordinates_from_csv'

### Creating Output Directory if it doesn't exist ###
if (!dir.exists(output_directory)) {
  dir.create(output_directory, recursive = TRUE)  # Create directory and any necessary parent directories
  cat("Directory created:", output_directory, "\n")
} 

### Reading v4 of the Industrial Facilities List ### 
inwu_facility_csv <- paste0(inwu_model_folder,
                            "/INWU_task_folders/Site_selection/Industrial_site_list/USEPA_HIFLD_EIA_PPP_facility_v6.csv")

# Read the Industrial Facilities List into a dataframe with specified column types
inwu_facility_df <- read_csv(inwu_facility_csv,
                             col_types = cols(
                               LATITUDE = col_double(),
                               LONGITUDE = col_double(),  # Fixed typo: LONGTITUDE -> LONGITUDE
                               NAICS = col_character(),
                               EMP = col_integer(),
                               TotPerson = col_integer(),
                               JobsReported = col_integer()
                             )) %>%
  mutate(CDC_Region = NA)

### Getting List of CSV File Outputs from `Geo_code_Add_lat_Long.R` ###
csv_directory <- paste0(inwu_model_folder, 
                        '\\INWU_task_folders\\Site_selection\\Data\\_CLEANUP\\Find_Lats_Longs2')

# List all CSV files in the specified directory
csv_files <- list.files(path = csv_directory, 
                        full.names = TRUE)

### Merging Outputs with v4 of the INWU Facility List ###
updated_inwu_facility_df <- inwu_facility_df
na_count <- sum(is.na(inwu_facility_df$LATITUDE))  # Count NA latitude values before updates

# Loop through each geocoding output file
for (csv in csv_files) {
  
  csv_name <- basename(csv)
  source <- strsplit(csv_name, "_")[[1]][3]
  
  if (source == 'LL') {
    source = NA
  }
  
  # Skip files where the source is 'No', as these indicate null values
  skip_list <- c('No')
  if (!(source %in% skip_list)) {
    
    geocode_df <- read_csv(csv)  # Read the geocoded data
    print(csv_name)  # Print the name of the current CSV file being processed
    
    # Update facility coordinates using the geocoded data
    updated_inwu_facility_df <- update_facility_coordinates(geocode_df, 
                                                            updated_inwu_facility_df, 
                                                            source)
  }
}

### Handling Duplicate Facility Entries ###

# Identify duplicate facilities based on FACILITYID
duplicate_facilities <- inwu_facility_df %>%
  group_by(FACILITYID) %>%
  summarise(count = n()) %>%
  filter(count > 1)

# Extract the list of duplicate facility IDs
duplicate_facilities_list <- duplicate_facilities$FACILITYID

# Create a dataframe of the duplicate facilities
duplicate_facilities_df <- inwu_facility_df %>%
  filter(FACILITYID %in% duplicate_facilities_list)

# Remove duplicate facilities, keeping only the first occurrence of each FACILITYID
updated_inwu_facility_df <- updated_inwu_facility_df %>%
  distinct(FACILITYID, .keep_all = TRUE)

### Getting Counts of Updates ###

# Count the number of NA values in the updated dataframe
updated_na_count <- sum(is.na(updated_inwu_facility_df$LATITUDE))

# Calculate the number of records that were updated
updated_record_count <- na_count - updated_na_count

# Print the number of coordinates that were updated
print(paste0(updated_record_count, ' coordinates updated'))

# Count occurrences of different sources in the updated dataframe
coordinate_sources <- updated_inwu_facility_df %>%
  group_by(LL_Src) %>%
  summarise(count = n())

# Outputting Results to CSV Files
output_path <- paste0(output_directory, '/USEPA_HIFLD_EIA_PPP_facility_v7.csv')
duplicates_path <- paste0(output_directory, '/duplicate_facility_ids_v4_20241114.csv')

# Write the updated facilities dataframe to CSV
write.csv(updated_inwu_facility_df, output_path, row.names = FALSE)
# Write the dataframe of duplicate facilities to CSV
write.csv(duplicate_facilities_df, duplicates_path, row.names = FALSE)