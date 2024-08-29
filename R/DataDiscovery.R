
# Title: Data Discover Functions
# Description: This script contains utility functions for handling and processing state data files, generating blank CSVs,
# and updating crosswalks for data directories.
# Author: Cathy Chamberlin (cchamberlin@usgs.gov) & Lily Gorman Sanisaca (lgormansanisaca@usgs.gov)
# Date: 8/22/24
#
# This script provides functions to list, read, and catalogue state data files, generate blank CSVs,
# and update crosswalks for data directories.

# Load necessary packages
library(purrr)
library(dplyr)
library(stringr)
library(readxl)
library(svDialogs)

#' List State Data Files
#'
#' This function lists all state data files in the specified directory
#'
#' @param stDatadir A character string representing the directory containing state data files.
#' @return A character vector of the names of state data files found.
#' @export
listSTdata <- function(stDatadir) {
  headdir <- dirname(stDatadir)
  
  stDatafp <- list.files(stDatadir, full.names = TRUE, recursive = TRUE)
  
  # Test if directories exists
  findDirs <- sapply(stDatafp, dir.exists)
  stDatafp <- stDatafp[!findDirs]
  
  fps <- stDatafp
  filenames <- lapply(fps, basename)
  if(any(grepl(".zip|.7z", fps))) {
    tmp <- tempfile()
    purrr::map(fps[grepl(".zip|.7z", fps)], ~archive::archive_extract(.x, dir = tmp))
    zipfls <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    flnms <- unlist(stringr::str_extract(zipfls, "(?<=/).*"))
    
    filenames <- c(filenames[-grep(".zip|.7z", fps)], flnms)
    fps <- c(fps[-grep(".zip|.7z", fps)], zipfls)
  }
  
  names(fps) <- filenames
  
  fps <- purrr::map(stDatafp,
                    ~ {
                      fp <- .x
                      filenames <- if(grepl("\\~\\$", fp)) {
                        name <- fp
                      } else if(
                        grepl(".csv|.txt", fp)) {
                        name <- fp
                      } else if(grepl(".xlsx|.xls", fp)) {
                        name <- paste(fp, readxl::excel_sheets(fp), sep = "$")
                      } else {
                        name <- fp
                      }
                    })
  
  dat <- gsub(headdir, "", unlist(fps))
  return(dat)
  
}

#' List Data Directories
#'
#' This function lists all relevant state data directories within a specified path.
#'
#' @param fp A character string representing the main directory to search for state data.
#' @return A character vector of the found state data directories.
#' @export
listdatadirs <- function(fp) {
  states <- state.abb
  subdirs <- subset(list.dirs(fp), stringr::str_extract(list.dirs(fp), "(?<=state_data/).*") %in% states)
  return(subdirs)
}

#' Get Data File Path
#'
#' This function prompts the user to select the location of the formatted state data.
#'
#' @return A character string representing the directory of the formatted state data.
#' @export
getdatafp <- function() {
  path_to_remote_tracker <- svDialogs::dlg_open(
    title = "Please select the file `NonSWUDS_Data_Input_Tracking.xlsx` from the folder `state_data`:"
  )$res
  
  
  unformattedstatedata <- dirname(path_to_remote_tracker)
  return(unformattedstatedata)
}

#' Get All Data from Directories
#'
#' This function retrieves all data locations from the specified folder structure.
#'
#' @param fp A character string representing the directory containing state data. 
#' @return A character vector of all available data files found.
#' @export
get_all_dat <- function(fp) {
  if(!exists("fp")) {
    fp <- getdatafp()
  }
  availdat <- listdatadirs(fp)
  unlist(purrr::map(availdat, ~listSTdata(.x)))
}

#' Generate a Blank CSV Data Frame
#'
#' This function creates a blank data frame structure suitable for CSV output.
#' The data frame contains predefined columns initialized to NA for each column.
#'
#' @param x A character vector of filenames to be included in the blank data frame.
#' @return A data frame initialized with empty values for each column, including:
#' - file: filenames passed as input
#' - SiteDescriptions, LocationInfo, MonthlyData, AnnualData, Metadata,
#' - Duplicate, NotRelevant: all initialized to NA.
#' @export
generate_blankcsv <- function(x) {
  blankdat <- data.frame(
    file = x,
    SiteDescriptions = NA,
    LocationInfo = NA,
    MonthlyData = NA,
    AnnualData = NA,
    Metadata = NA,
    Duplicate = NA,
    NotRelevant = NA
  )
  
  return(blankdat)
}

#' Generate a Blank Header Crosswalk CSV
#'
#' This function generates a blank data frame for the Header Crosswalk based on 
#' the filled assignment data from the existing DataDirectories file
#'
#' @param filledassignment A data frame containing the assignment data which 
#' has been filled in.
#' @return A data frame structured for the Header Crosswalk with NA values for 
#' additional columns.
#' @export
generate_blankHeaderCrosswalkcsv <- function(filledassignment) {
  
  filledfile <- filledassignment
  
  blanksiteDescripts <- filledfile |> dplyr::filter(SiteDescriptions == 1 | 
                    LocationInfo == 1 |
                    MonthlyData == 1 |
                    AnnualData == 1 |
                    Metadata == 1) |>
    dplyr::mutate(State = stringr::str_extract(file, "(?<=/)[[:alpha:]]{2}")) |>
    dplyr::select(State, file) |>
    dplyr::mutate(IsReadMe = NA) |>
    dplyr::mutate(ValueType = NA, SourceType = NA, Category = NA, Saline = NA, 
                  FacilityName = NA, FacilityName1 = NA, FacilityName2 = NA,
                  FacilityNumber = NA, FacilityNumber1 = NA, FacilityNumber2 = NA,
                  SourceName = NA, SourceName1 = NA, SourceName2 = NA,
                  SourceNumber = NA, SourceNumber1 = NA, SourceNumber2 = NA, 
                  NAICS = NA, SIC = NA, Description = NA, HUC8 = NA, HUC10 = NA, 
                  HUC12 = NA, AquiferName1 = NA, AquiferName2 = NA,
                  BasinName1 = NA, BasinName2 = NA, Address1 = NA, City1 = NA,
                  County1 = NA, State1 = NA, Zip1 = NA, Address2 = NA, City2 = NA,
                  County2 = NA, State2 = NA, Zip2 = NA, Lat = NA, Lon = NA, Datum = NA, 
                  Projection = NA, Year = NA, Jan = NA, Feb = NA, Mar = NA, Apr = NA,
                  May = NA, Jun = NA, Jul = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA,
                  Dec = NA, Units_monthly = NA, Method_monthly = NA, Annual_reported = NA,
                  Units_annual_reported = NA, Method_annual_reported = NA, DataProtected = NA)
  
  return(blanksiteDescripts)
}


#' Read a Filled CSV File
#'
#' This function reads a CSV file and returns its content as a data frame.
#'
#' @param file A character string representing the path to the CSV file to be read.
#' @return A data frame containing the contents of the CSV file.
#' @export
get_filledcsv <- function(file) {
  read.csv(file, colClasses = "character")
}


#' Merge Blank and Filled Data
#'
#' This function merges a blank data frame with a filled data frame.
#'
#' @param blank A data frame representing the blank template.
#' @param filled A data frame representing the filled data.
#' @return A combined data frame that merges the blank and filled data, 
#' ensuring uniqueness of the resulting rows.
#' @export
merge_data <- function(blank, filled) {
  filledfile <- filled
  fp_classified <- filledfile |> na.omit() |> dplyr::pull(file)
  
  fp_unclassified <- blank |> dplyr::filter(!file %in% fp_classified) |> dplyr::mutate(across(everything(), ~as.character(.)))
  
  d <- dplyr::bind_rows(fp_unclassified, filledfile) |> unique()
  return(d)
}

#' Update Crosswalks
#'
#' This function updates the crosswalks by reading existing CSV files, merging 
#' new data from the provided data frame, and writing updated CSV files back 
#' to the specified directory.
#'
#' @param data A data frame containing the new data to incorporate into existing crosswalks.
#' @param existingCrosswalks A character string representing the directory where existing 
#' crosswalk files are stored.
#' @return A list containing updated crosswalks, including DataDirectories and HeaderCrosswalk.
#' @export
updateCrosswalks <- function(data, existingCrosswalks) {
  existingCrosswalks_read <- map(
    list.files(existingCrosswalks, pattern = ".csv", full.names = TRUE), 
    ~get_filledcsv(.x)); names(existingCrosswalks_read) <- 
      gsub(".csv", "", list.files(existingCrosswalks, pattern = ".csv"))
  
  existingStateForms <- map(
    list.files(file.path(existingCrosswalks, "StateForms"), pattern = ".csv", 
               full.names = TRUE), 
    ~read.table(.x, colClasses = "character", sep = ",")); names(existingStateForms) <- 
    gsub(".csv", "", list.files(file.path(existingCrosswalks, "StateForms"), 
                                                           pattern = ".csv"))
  
  existingCrosswalks_read$Forms <- existingStateForms
  
  updatedCrosswalks <- existingCrosswalks_read
  
  blankDataDictionary <- generate_blankcsv(data)
  updatedDataDictionary <- merge_data(blank = blankDataDictionary, 
                                      filled = existingCrosswalks_read$DataDirectories)
  write.csv(updatedDataDictionary, 
            file.path(existingCrosswalks, "DataDirectories.csv"), 
            row.names = FALSE)
  updatedCrosswalks$DataDirectories <- updatedDataDictionary
  
  blankHeaderCrosswalk <- generate_blankHeaderCrosswalkcsv(updatedDataDictionary)
  updatedHeaderCrosswalk <- merge_data(blank = blankHeaderCrosswalk, filled = existingCrosswalks_read$HeaderCrosswalk)
  write.csv(updatedHeaderCrosswalk, 
            file.path(existingCrosswalks, "HeaderCrosswalk.csv"), 
            row.names = FALSE) 
  updatedCrosswalks$HeaderCrosswalk <- updatedHeaderCrosswalk
  
  unprocessedstates <- unique(c(
    flag_unprocessedstates(updatedCrosswalks$DataDirectories),
    flag_unprocessedstates(updatedCrosswalks$HeaderCrosswalk)))
  
  if(length(unprocessedstates > 0)) {
    message(paste("Unprocessed data remains for the following state(s):", paste(unprocessedstates, collapse = ", ")))}
  
  return(updatedCrosswalks)
}

flag_unprocessedstates <- function(x) {
  states <- x %>% filter(if_any(.cols = everything(), .fns = ~is.na(.))) %>%
    pull(file) %>% str_extract(., "(?<=/)[[:alpha:]]{2}(?=/)") %>% unique()
  return(states)
}
