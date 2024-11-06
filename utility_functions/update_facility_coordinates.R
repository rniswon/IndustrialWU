#' @title update_facility_coordinates.R
#' @description This function mergest coordinates from Judy's Geo_code_Add_lat_Long.R
#' to our facilities list
#' @param coordinate_df 
#' @param source
#' @@return a dataframe with updated lat longs

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
  
  updated_records <- na_count - updated_na_count
  
  print(paste0(updated_record_count, ' coordinates updated'))
  
}