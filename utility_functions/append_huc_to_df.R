#' Append HUC Information to an SF Object
#'
#' This function reads a dataframe containing facility information, filters out 
#' entries with missing longitude values, converts it to an sf object, and then 
#' joins HUC (Hydrologic Unit Code) information from a specified layer in a 
#' GeoPackage. The function returns a dataframe with the HUC information appended.
#'
#' @param input_df A dataframe containing facility data with columns for 
#'        LONGITUDE and LATITUDE, and a FACILITYID for joining.
#' @param hu_no An integer representing the HUC level to be used (e.g., 2 for HUC2).
#' @param wbd_gpkg_pth A string representing the file path to the GeoPackage 
#'        containing the WBD (Watershed Boundary Dataset) layers.
#'
#' @return A dataframe that includes the original data from input_df along with 
#'         the HUC information appended based on the FACILITYID.
#'
#' @import dplyr
#' @import sf
#'
#' @examples
#' # Example usage:
#' result <- append_hu_to_sf(input_df, hu_no = 2, wbd_gpkg_pth = "path/to/wbd.gpkg")
#'
append_hu_to_sf <- function(input_df, hu_no, wbd_gpkg_pth) {
  
  # Load the required library for spatial operations
  library(sf)
  
  # Construct the layer name and selection field based on the provided HUC number
  wbd_layer_name <- paste0('WBDHU', as.character(hu_no))
  selection_field <- paste0('huc', as.character(hu_no))
  
  # Filter out rows from input_df where LONGITUDE is NA
  input_df <- input_df %>%
    filter(!is.na(LONGITUDE))
  
  # Convert the filtered input dataframe to an sf object using LONGITUDE and LATITUDE as coordinates
  input_sf <- st_as_sf(input_df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  # Read the specified WBD layer from the GeoPackage and transform it to the same CRS (EPSG:4326)
  wbd_sf <- st_read(dsn = wbd_gpkg_pth, layer = wbd_layer_name) %>%
    st_transform(crs = 4326)
  
  # Perform a spatial join to add the HUC information to the input sf object
  input_sf <- st_join(input_sf, wbd_sf[, c(selection_field)], join = st_within)
  
  # Join the original input_df with the HUC information based on FACILITYID and remove geometry
  result_df <- input_df %>%
    left_join(input_sf %>% select(FACILITYID, !!selection_field), by = "FACILITYID") %>%
    select(-geometry)  # Remove the geometry column from the result
  
  # Return the resulting dataframe with the HUC information appended
  return(result_df)
}
