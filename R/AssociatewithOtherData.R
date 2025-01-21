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
  # The reason is that this file is so large, it is helpful to have a separate target for it.
  # It also allows more specificity in how this file is handled
  # The headers from the national Header Crosswalk are used to read and rename the data
  # Then, the facility names are cleaned, and the address columns are formatted.
  # These adjustments will help with later merging.
  # browser()
  natHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv")),
    DataCodesCrosswalk = datacodes_Xwalks
  )
  siteSelectionDat <- readandrename_columns(siteselection, natHeaders, national_Xwalks, data = "National") |>
    pluck(1) |>
    tidytable::mutate(tidytable::across(c("FacilityName", "FacilityName1", "FacilityName2"), ~clean_names(.))) |>
    tidytable::mutate(tidytable::across(c("Address1", "Address2"), ~clean_address_words(.))) |>
    tidytable::mutate(tidytable::across(c("City1", "County1", "City2"), ~str_to_title(.))) |>
    tidytable::mutate(State = State1) |>
    unique() |>
    as.data.frame()
    
  return(siteSelectionDat)
}

prep_nationaldata <- function(national_Xwalks, datacodes_Xwalks, existingCrosswalks, natdata = list(), extradata = list()) {
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
    merge_formatteddata(., natHeaders, existingCrosswalks, data = "National") %>%
    dplyr::filter(!is.na(FacilityName)) %>%  dplyr::mutate(State = State1) %>%
    standard_Addresstreatment(., "natData", "State")
  
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
                                   unique(WUdata[[mergevars[[1]]]])) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(mergevars), is.na))
  merge_dat <- rquery::natural_join(WUdata, siteselection_subset, by = mergevars, jointype = "LEFT") 
  if(nrow(merge_dat) != nrow(WUdata)) {
    nadd <- nrow(merge_dat) - nrow(WUdata)
    m <- paste("Warning:", nadd,"Duplicates added when merging by", paste(mergevars, collapse = ", "))
    message(m) 
    # browser()
    } # something went wrong
  merge_success <- merge_dat %>% dplyr::filter(!is.na(SITESELECTION_FACILITYID)) %>% 
    dplyr::group_by(dplyr::across(all_of(names(WUdata)))) %>% 
    dplyr::summarise(dplyr::across(contains("SITESELECTION"), ~paste(unique(.), collapse = " _OR_ ")),
              .groups = "drop") 
    m <- paste("Merging by", paste(mergevars, collapse = ", "), "produced", nrow(merge_success),  "matches")
  message(m)
  
  merge_fail <- merge_dat %>% dplyr::filter(is.na(SITESELECTION_FACILITYID)) %>% 
    dplyr::select(all_of(names(WUdata)))
  
  return(list(merge_success = merge_success, merge_fail = merge_fail))
}

merge_siteselection <- function(data, siteselection, siteselectionfilename) {
  # browser()
# the site selection data is merged several times by various characteristics
  # this is intented to maximize the number of merged lines
  
  mergevar_synonyms <- list(
    name = c("FacilityName", "FacilityName1", "FacilityName2"),
    address = c("Address1", "Address2"),
    city = c("City1", "City2"),
    county = c("County1"), #County2 not used. Try adding again at the end
    state = c("State", "State1") #State2 not used. Try adding again at the end
  )
  
  level1_mergevars <- mergevar_synonyms$state
  level2_mergevars <- crossing(mergevar_synonyms$name, mergevar_synonyms$address)
  level3_mergevars <- crossing(mergevar_synonyms$county, mergevar_synonyms$city)
  
  level2merge <- list_flatten(
    list(
      list_flatten(map(unique(unlist(level2_mergevars)), ~{l2 <- .x; map(level1_mergevars, ~c(l2, .x))})),
      list_flatten(pmap(level2_mergevars, ~{l2 <- c(.x, .y); map(level1_mergevars, ~c(l2, .x))}))
    )
  )
  
  level3amerge <- list_flatten(
    map(
      unique(unlist(level3_mergevars)), 
      ~{
        l3 <- .x
        list_flatten(map(unique(unlist(level2_mergevars)),  ~{l2 <- .x; map(level1_mergevars,~c(l3, l2, .x))}))
      }
    )
  )
  level3bmerge <- list_flatten(
    map(
      unique(unlist(level2_mergevars)), 
      ~{
        l2 <- .x
        list_flatten(pmap(level3_mergevars, ~{l3 <- c(.x, .y); map(level1_mergevars, ~c(l3, l2, .x))}))
      }
    )
  )
  level3cmerge <- list_flatten(
    pmap(
      level2_mergevars, 
      ~{
        l2a <- .x
        l2b <- .y
        list_flatten(pmap(
          level3_mergevars,
          ~{l3 <- c(.x, .y, l2a, l2b); map(level1_mergevars, ~c(l3, .x))}
        ))
      }
    )
  )
  level3merge <- list_flatten(list(level3amerge, level3bmerge, level3cmerge))
  
  merge_combos <- list_flatten(list(level2merge = level2merge, level3merge = level3merge))
  
  merge_combos_indices_bysize <- unlist(purrr::map(merge_combos, ~length(.x))) |> sort(decreasing = TRUE)
  merge_combos_ordered <- merge_combos[names(merge_combos_indices_bysize)] 
  
  merge_success <- list()
  merge_fail <- list()
  
  for(i in 1:length(merge_combos_ordered)) {
    if(i == 1) {
      # the first merge needs to take the WU data as the base
      merge_tmp <- iterative_merge_siteselection(data, siteselection, merge_combos_ordered[[i]])
    } else {
      merge_tmp <- iterative_merge_siteselection(merge_fail[[i-1]], siteselection, merge_combos_ordered[[i]])
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