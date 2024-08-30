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
   reformat_data(., natHeaders, national_Xwalks, data = "National") %>%
   filter(!is.na(FacilityName)) %>% mutate(State = State1) %>%
    standard_Addresstreatment(., "State")
 
 nonSWUDSwNat <- merge_andreplaceNA(mutate(nonSWUDS, Source = "NonSWUDS"), natData) |> 
   dplyr::select(State, any_of(names(natHeaders$HeaderCrosswalk)), DataSource)
 
 return(nonSWUDSwNat)

}
