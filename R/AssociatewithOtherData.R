# Title: Data Combining Functions
# Description: This script contains functions for combining SWUDS and other national datasets with the nonSWUDS data
# Author: Cathy Chamberlin (cchamberlin@usgs.gov)
# Date: 8/22/24

#' Merge National Data
#'
#' This function merges national data from a specified source with existing data. It 
#' reads and renames columns based on provided crosswalks, reformats the data, 
#' filters out rows with NA `FacilityName`, and then merges it with another dataset.
#'
#' @param nonSWUDS A data frame that contains non-SWUDS data to be merged.
#' @param national_Xwalks A string representing the path to national crosswalk files.
#' @param datacodes_Xwalks A data frame containing data codes crosswalks.
#' @param natdata A list containing national data to be processed.
#'
#' @return A merged data frame that combines non-SWUDS data with national data.
#'
#' @examples
#' \dontrun{
#'   merged_data <- merge_nationaldata(non_swuds_df, "path/to/national_xwalks", data_codes_xwalk_df, national_data_list)
#'   }
#'
merge_nationaldata <- function(nonSWUDS, national_Xwalks, datacodes_Xwalks, natdata = list()) {
  natHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv")),
    DataCodesCrosswalk = datacodes_Xwalks
    )
  if(any(!str_detect(paste(natHeaders$HeaderCrosswalk$file, collapse = ""), 
                     basename(unlist(natdata))))) {
     unxwalked <- natdata[!str_detect(paste(natHeaders$HeaderCrosswalk$file, collapse = ""), 
                 basename(unlist(natdata)))]
     stop(paste0("Headers Need to be Crosswalked for ", unxwalked))
  }
 natData <- readandrename_columns(natdata, natHeaders, national_Xwalks, data = "National") %>%
   reformat_data(., natHeaders, national_Xwalks) %>%
   merge_formatteddata(., natHeaders, data = "National") %>%
   filter(!is.na(FacilityName)) %>%  mutate(State = State1) %>%
    standard_Addresstreatment(., "State")
 
 nonSWUDSwNat <- merge_andreplaceNA(mutate(nonSWUDS, Source = "NonSWUDS"), natData) |> 
   dplyr::select(State, any_of(names(natHeaders$HeaderCrosswalk)), DataSource)
 
 return(nonSWUDSwNat)

}

augment_data <- function(data, national_Xwalks, extradata = list()) {
  extrasHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv"))
  )
  if(any(!str_detect(paste(extrasHeaders$HeaderCrosswalk$file, collapse = ""), 
                     basename(unlist(extradata))))) {
    unxwalked <- extradata[!str_detect(paste(extrasHeaders$HeaderCrosswalk$file, collapse = ""), 
                                     basename(unlist(extradata)))]
    stop(paste0("Headers Need to be Crosswalked for ", unxwalked))
  }
  
  augmentedData <- readandrename_columns(extradata, extrasHeaders, national_Xwalks, data = "National") |>
    reformat_data(extrasHeaders, national_Xwalks) %>%
    map(., ~filter(.x, !is.na(County1))) %>%
    map(., ~mutate(.x, across(any_of("State"), ~State1))) %>%
    map(., ~standard_Addresstreatment(.x, "State")) %>% 
    purrr::reduce2(.x = ., .y = names(.), .f = merge_fips, 
                   .init = data) |> 
      dplyr::select(State, any_of(names(extrasHeaders$HeaderCrosswalk)), DataSource)
  
  return(augmentedData)
}

merge_fips <- function(x, y, yname) {
  merge_andreplaceNA(x = x, y = y, yname = yname, merge_vars = c("County1", "State"), jointype = "LEFT")
}
