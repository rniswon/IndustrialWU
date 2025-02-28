#' @title Populate Facility Coordinates from County Centroid
#' @description This function populates missing facility coordinates by matching city and state information with a GNIS layer of county centroids.
#' 
#' @param input_df A data frame containing facility information, including CITY and STATE fields.
#' @param lat_field The name of the latitude field in the input data frame (default is 'LATITUDE').
#' @param long_field The name of the longitude field in the input data frame (default is 'LONGITUDE').
#' @param GNIS_layer The file path to the GNIS layer CSV which includes county centroid coordinates.
#' 
#' @return A data frame with updated facility coordinates, including resolved coordinates for those that were missing.
#' 
#' @details This function reads a GNIS layer, prepares it by selecting relevant fields, and then attempts to resolve missing coordinates for facilities by matching city and state information. It reports how many facilities had missing coordinates and how many were successfully resolved.

populate_facility_coords_cty_centroid <- function(input_df, lat_field, long_field, GNIS_layer) {
  
  # Preparing Counties Layer
  
  GNIS_df <- read_csv(GNIS_layer)
  
  GNIS_df <- GNIS_df %>%
    mutate(PP_NAME = str_to_upper(PP_NAME)) %>%
    select(PP_NAME, STATE_ABBV, LAT, LONG)
  
  # Preparing Facilities Layer
  
  facility_fields <- names(input_df)
  
  facilities_with_coords <- input_df %>%
    filter(!is.na(LATITUDE))
  
  facilities_missing_coords <- input_df %>%
    filter(is.na(LATITUDE))
  
  # Resolving coordinates
  
  unresolved_coords_count <- nrow(facilities_missing_coords)
  raw_facilities_count <- nrow(input_df)
  
  print(paste0(unresolved_coords_count,' of ', raw_facilities_count, ' missing cordinates'))
  
  facilities_resolved_coords_df <- facilities_missing_coords %>%
    inner_join(GNIS_df, by = c("CITY" = "PP_NAME", "STATE" = "STATE_ABBV")) %>%
    mutate(LATITUDE = coalesce(LATITUDE, LAT),
           LONGITUDE = coalesce(LONGITUDE, LONG),
           LL_Src = 'City Centroid') %>%
    select(facility_fields) %>%
    distinct(FACILITYID, .keep_all = TRUE)
  
  resolved_coordinates <- nrow(facilities_resolved_coords_df)
  
  print(paste0(resolved_coordinates, ' facility coordinates have been resolved'))
  
  input_df <- rbind(facilities_with_coords,
                    facilities_resolved_coords_df)
  
  return(input_df)
}