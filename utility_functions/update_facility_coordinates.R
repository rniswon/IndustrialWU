#' @title Update Facility Coordinates
#' @description This function merges coordinates from Judy's `Geo_code_Add_lat_Long.R`
#' to the provided facilities list. It updates the latitude and longitude for facilities 
#' based on their `FACILITYID`, using the coordinates from the `coordinate_df`.
#' 
#' @param coordinate_df A data frame containing facility coordinates with at least 
#'                      the columns `FACILITYID`, `lat`, and `long`.
#' @param facility_df A data frame containing facility information, which must include 
#'                    the columns `FACILITYID`, `LATITUDE`, `LONGITUDE`, and `LL_Src`.
#' @param source A character string indicating the source of the latitude and longitude 
#'               data being added. This will be recorded in the `LL_Src` column of 
#'               the `facility_df`.
#' 
#' @return A data frame with updated latitude and longitude values. The function 
#'         preserves all original columns in `facility_df` and updates `LATITUDE`, 
#'         `LONGITUDE`, and `LL_Src` where applicable.
#' 
#' @details The function counts the number of missing latitude values before and after 
#'          the update process, printing the number of coordinates that were updated. 
#'          If a facility's latitude and longitude are already available, they will 
#'          remain unchanged.

update_facility_coordinates <- function(coordinate_df, facility_df, source){
  
  library(tidyverse)
  
  facility_fields <- names(facility_df)
  
  coordinate_df <- coordinate_df %>%
    select(FACILITYID, lat, long) %>%
    mutate(source = source)

  na_count <- sum(is.na(facility_df$LATITUDE))
  
  facility_df <- facility_df %>%
    mutate(LL_Src = ifelse(is.na(LATITUDE) & is.na(LONGITUDE), NA, LL_Src)) %>%
    left_join(coordinate_df, by = "FACILITYID") %>%
    mutate(
      LATITUDE = coalesce(LATITUDE, lat),
      LONGITUDE = coalesce(LONGITUDE, long),
      LL_Src = coalesce(LL_Src, source)
    ) %>%
    select(facility_fields)
  
  updated_na_count <- sum(is.na(facility_df$LATITUDE))
  
  updated_record_count <- na_count - updated_na_count
  
  print(paste0(updated_record_count, ' coordinates updated'))
  
  return(facility_df)
  
}

#' @examples
# Load necessary libraries
# library(readr)
# 
# # Define file paths for facility and coordinate data
# inwu_model_folder <- "D:/DOI/GS-W-WaterUse - Industrial model"
# inwu_facility_csv <- paste0(inwu_model_folder,
#                             "/INWU_task_folders/Site_selection/Industrial_site_list/USEPA_HIFLD_EIA_PPP_facility_v4.csv")
# coordinate_csv <- paste0(inwu_model_folder,
#                          "/INWU_task_folders/Site_selection/Data/_CLEANUP/NAICS_321_Wood_census_geo.csv")
# 
# # Read in the data
# facility_df <- read_csv(inwu_facility_csv)
# coordinate_df <- read_csv(coordinate_csv)
# 
# # Update facility coordinates
# updated_facility_df <- update_facility_coordinates(coordinate_df, facility_df, 'Census')

#' @import tidyverse
#' @export