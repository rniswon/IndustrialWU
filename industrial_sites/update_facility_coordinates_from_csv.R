# ---------------------------------------------------------------------------
# R Script: Update Facility Coordinates
# ---------------------------------------------------------------------------
#
# Purpose:
# This script updates the geographical coordinates (latitude and longitude) 
# of industrial facilities based on additional geocoding data. It reads a 
# list of facilities, merges it with geocoded data, and updates the 
# coordinates where applicable.
#
# Dependencies:
# - utility_functions/update_facility_coordinates.R: Contains the function 
#   `update_facility_coordinates` used for updating facility coordinates.
#
# User Inputs:
# - inwu_model_folder: Path to the Industrial model folder.
# - output_directory: Directory for output files (currently not set).
#
# Data Sources:
# 1. Industrial Facilities List (v4):
#    - File: USEPA_HIFLD_EIA_PPP_facility_v4.csv
#    - Location: D:/DOI/GS-W-WaterUse - Industrial model/INWU_task_folders/
#      Site_selection/Industrial_site_list/
#    - Columns:
#      - LATITUDE: Double
#      - LONGTITUDE: Double
#      - NAICS: Character
#      - EMP: Integer
#      - TotPerson: Integer
#      - JobsReported: Integer
#
# 2. Geocoding Outputs:
#    - Location: ~/GS-W-WaterUse - Industrial model/INWU_task_folders/
#      Site_selection/Data/_CLEANUP/Find_Lats_Longs/NAICS_Address
#    - Files are the ourputs of `Geo_code_Add_lat_Long.R` 
#    - Files are named in the format: NIACS_[3-digit Code]_[Category Name]_
#      [Source]_geo.csv
#
# Processing Steps:
# 1. Read the Industrial Facilities List.
# 2. List CSV files from geocoding output.
# 3. Update facility coordinates by merging with geocoded data.
# 4. Count and print the number of updated records.
#
# Output:
# - Prints the number of coordinates that were updated.


source("utility_functions/update_facility_coordinates.R")

### Getting User Inputs ###
inwu_model_folder <- "D:/DOI/GS-W-WaterUse - Industrial model"
output_directory <- ''
  

### Reading v4 of the Industrial Facilities List ### 
inwu_facility_csv <- paste0(inwu_model_folder,
                            "/INWU_task_folders/Site_selection/Industrial_site_list/USEPA_HIFLD_EIA_PPP_facility_v4.csv")

inwu_facility_df <- read_csv(inwu_facility_csv,
                             col_types = cols(
                               LATITUDE = col_double(),
                               LONGTITUDE = col_double(),
                               NAICS = col_character(),
                               EMP = col_integer(),
                               TotPerson = col_integer(),
                               JobsReported = col_integer(),
                             ))

### Getting List of CSV File outputs from `Geo_code_Add_lat_Long.R` ###
csv_directory <- paste0(inwu_model_folder, 
                        '\\INWU_task_folders\\Site_selection\\Data\\_CLEANUP\\Find_Lats_Longs\\NAICS_Address')

csv_files <- list.files(path = csv_directory, 
                        full.names = TRUE)

### Merging 'GEO_code_Add_lat_Long.R outputs with v4 of the inwu facility list
updated_inwu_facility_df <- inwu_facility_df

na_count <- sum(is.na(inwu_facility_df$LATITUDE))

for (csv in csv_files){
  
  # The .csv file outputs from `Geo_code_Add_lat_Long.R` are in the format 
  ## NIACS_[3-digit Code]_[Category Name]_[Source]_geo.csv
  ## Extracting the source from the filename
  csv_name <- basename(csv)
  source <- strsplit(csv_name, "_")[[1]][4]
  
  # `Geo_code_Add_lat_Long.R` has a source of 'No' for her null values 
  # the census source currently has multiple lat long fields, skipping for now
  ## skipping those
  skip_list <- c('No', 'census')
  if (!(source %in% skip_list)){
    
    geocode_df <- read_csv(csv)
    names <- names(geocode_df)
    print(csv_name)
    print(names)
    
    updated_inwu_facility_df <- update_facility_coordinates(geocode_df, 
                                                             updated_inwu_facility_df, 
                                                             source)
    
  }
  
}

updated_na_count <- sum(is.na(updated_inwu_facility_df$LATITUDE))

updated_record_count <- na_count - updated_na_count

print(paste0(updated_record_count, ' coordinates updated'))
