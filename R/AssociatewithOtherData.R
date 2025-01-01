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
merge_nationaldata <- function(nonSWUDS, natData, natHeaders) {
  
  
  # this function merges the national-scale data with the non SWUDs data. This is done before adding site selection data.
 nonSWUDSwNat <- merge_andreplaceNA(dplyr::mutate(nonSWUDS, Source = "NonSWUDS"), natData) |> 
   dplyr::select(State, any_of(names(natHeaders$HeaderCrosswalk)), DataSource)
 
 return(nonSWUDSwNat)

}

prep_siteselection <- function(national_Xwalks, datacodes_Xwalks, siteselection = list()) {
  # The site selection data is formatted separately from the other national data
  # The reason is that this file is so large, it is helpful to have a separate target for it
  # The headers from the national Header Crosswalk are used to read and rename the data
  # Then, the facility names are cleaned, and the address columns are formatted.
  # These adjustments will help with later merging.
  natHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv")),
    DataCodesCrosswalk = datacodes_Xwalks
  )
  siteSelectionDat <- readandrename_columns(siteselection, natHeaders, national_Xwalks, data = "National") |>
    pluck(1) |>
    dplyr::mutate(
      FacilityName = fedmatch::clean_strings(
        as.character(FacilityName), common_words = fedmatch::corporate_words)
      ) |>
    dplyr::mutate(dplyr::across(c("Address1", "City1", "County1"), ~str_to_title(.))) |>
    dplyr::mutate(State = State1) |>
    unique()
    
  return(siteSelectionDat)
}

prep_nationaldata <- function(national_Xwalks, datacodes_Xwalks, natdata = list(), extradata = list()) {
  # This function preps national scale data, such as the FIPS codes, Lauren's work with updated locations, etc
  # The headers from the national crosswalk are used to read and rename the national files
  # The national files are all merged together first before they are merged with the water use data.
  # The difference between the "extra data" and the "national data", are that the national data may include extra sites.
  # The extra data includes fields that may be used to augment the national data, but it won't include new sites.
  natHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv")),
    DataCodesCrosswalk = datacodes_Xwalks
  )
  if(any(!str_detect(paste(natHeaders$HeaderCrosswalk$file, collapse = ""), 
                     c(basename(unlist(natdata)), basename(unlist(extradata)))))) {
    unxwalked <- natdata[!str_detect(paste(natHeaders$HeaderCrosswalk$file, collapse = ""), 
                                     basename(unlist(natdata)))]
    stop(paste0("Headers Need to be Crosswalked for ", unxwalked))
  }
  natData <- readandrename_columns(natdata, natHeaders, national_Xwalks, data = "National") %>%
    reformat_data(., natHeaders, national_Xwalks) %>%
    merge_formatteddata(., natHeaders, data = "National") %>%
    dplyr::filter(!is.na(FacilityName)) %>%  dplyr::mutate(State = State1) %>%
    standard_Addresstreatment(., "State")
  
  augmentData <- readandrename_columns(extradata, natHeaders, national_Xwalks, data = "National") |>
    reformat_data(natHeaders, national_Xwalks) %>%
    map(., ~dplyr::mutate(.x, dplyr::across(any_of("State"), ~State1)))
  
  return(list(natData_merge = natData, natHeaders = natHeaders, augmentData = augmentData))
}

augment_data <- function(data, data_to_add, natHeaders) {
  # this function prepares the data sets that will be used to augment the non SWUDS data
  augmentedData <- data_to_add %>%
    purrr::reduce2(.x = ., .y = names(.), .f = merge_withaugmentation, 
                   .init = data) |> 
      dplyr::select(State, any_of(names(natHeaders$HeaderCrosswalk)), DataSource)
  
  return(augmentedData)
}

merge_withaugmentation <- function(x, y, yname) {
  merge_vars <- names(y)[names(y) %in% names(x)]
  merge_andreplaceNA(x = x, y = y, yname = yname, merge_vars = merge_vars, jointype = "LEFT")
}

iterative_merge_siteselection <- function(WUdata, siteselectiondata, mergevars) {
  # this function does the merging of the site selection data into the WU data. 
  # the merge is a left merge, so ideally no new lines will be added
  # in practice, sometimes duplicates are created. 
  # there is a warning produced that indicates when duplicates are created so that they can be addressed in future updates
  # browser()
  siteselection_subset <- dplyr::filter(siteselectiondata, !!sym(mergevars[[1]]) %in% 
                                   unique(WUdata[[mergevars[[1]]]]))
  merge_dat <- rquery::natural_join(WUdata, siteselection_subset, by = mergevars, jointype = "LEFT") 
  if(nrow(merge_dat) != nrow(WUdata)) {
    nadd <- nrow(merge_dat) - nrow(WUdata)
    m <- paste("Warning:", nadd,"Duplicates added")
    message(m) 
    } # something went wrong
  merge_success <- merge_dat %>% dplyr::filter(!is.na(SITESELECTION_FACILITYID)) %>% 
    dplyr::group_by(dplyr::across(all_of(names(WUdata)))) %>% 
    dplyr::summarise(dplyr::across(contains("SITESELECTION"), ~paste(unique(.), collapse = " _OR_ ")),
              .groups = "drop") 
  merge_fail <- merge_dat %>% dplyr::filter(is.na(SITESELECTION_FACILITYID)) %>% 
    dplyr::select(all_of(names(WUdata)))
  
  return(list(merge_success = merge_success, merge_fail = merge_fail))
}

merge_siteselection <- function(data, siteselection, siteselectionfilename) {
  
  
# the site selection data is merged several times by various characteristics
  # this is intented to maximize the number of merged lines
  mergevars <- list(
    merge1 = c("FacilityName", "Address1", "City1", "County1", "State1"),
    merge2 = c("FacilityName", "City1", "County1", "State1"),
    merge3 = c("Address1", "City1", "County1", "State1"),
    merge4 = c("Address1", "City1", "State1"),
    merge5 = c("Address1", "County1", "State1"),
    merge6 = c("FacilityName", "City1", "State1"),
    merge7 = c("FacilityName", "County1", "State1"),
    merge8 = c("FacilityName", "Address1", "State1"),
    merge9 = c("FacilityName", "State1"),
    merge10 = c("Address1", "State1")
  )
  
  merge_success <- list()
  merge_fail <- list()
  
  for(i in 1:length(mergevars)) {
    if(i == 1) {
      # the first merge needs to take the WU data as the base
      merge_tmp <- iterative_merge_siteselection(data, siteselection, mergevars[[i]])
    } else {
      merge_tmp <- iterative_merge_siteselection(merge_fail[[i-1]], siteselection, mergevars[[i]])
    }
    merge_success[[i]] <- merge_tmp$merge_success
    merge_fail[[i]] <- merge_tmp$merge_fail
  }
  merge_success_all <- reduce(merge_success, dplyr::bind_rows) |>
    dplyr::mutate(DataSource = paste0(DataSource, ", ", basename(siteselectionfilename))) 
  merge_fail_all <- merge_fail[[length(merge_fail)]]
  alldat <- dplyr::bind_rows(merge_success_all, merge_fail_all) %>%
    arrange(State, FacilityName, SourceName, Year) %>%
    dplyr::select(all_of(names(data)), all_of(names(siteselection)), DataSource) |>
    dplyr::relocate(DataSource, .after = last_col())
  
  sites_wo_data <- siteselection %>% 
    dplyr::filter(!SITESELECTION_FACILITYID %in% alldat$SITESELECTION_FACILITYID)
  n_sites_nodata <- nrow(sites_wo_data)
  n_sites_data <- length(unique(merge_success_all$SITESELECTION_FACILITYID))
  n_datapoints <- nrow(merge_success_all)
  m <- paste(n_sites_data, 
             "sites from the site selection team have merged with data, for", 
             n_datapoints, "total data points, and", n_sites_nodata, 
             "sites have not merged with data.")
  message(m)
  
  return(alldat)
  }