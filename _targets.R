# Title: Targets Pipeline Script
# Description: This script defines a data processing pipeline using the targets package.
# Author: Cathy Chamberlin (cchamberlin@usgs.gov) & Lily Gorman Sanisaca (lgormansanisaca@usgs.gov)
# Date: 8/22/24
#
# Follow this script to set up your data processing pipeline using the targets library.


# Specify the packages required for the pipeline
# Note that tidytable and dplyr have many functions named the same. 
# Suggest specifying in the script dplyr:: or tidytable::
# dplyr is loaded last so that it is the default
packages <- c("tibble", "stringr", "purrr", "readxl", "svDialogs", "tidytable", 
              "dplyr", "archive", "tidyr", "readr", "lubridate", "magrittr", 
              "furrr", "data.table", "sf", "rquery", "officer", "pdftools", "rqdatatable",
              "fedmatch", "janitor", "zoo", "varhandle", "targets", "tarchetypes",
              "tigris")

# Function to install missing packages
optinstall <- function(x) {
  if (!x %in% utils::installed.packages()) {
    install.packages(x)  # Install package if it's not already installed
  }
}

# Suppress warnings and install any missing packages
suppressWarnings(invisible(lapply(packages, optinstall)))

# Load necessary packages
library(targets)
library(tarchetypes)
# Set options for the targets package
tar_option_set(packages = packages,  # Define required packages
               format = "rds")        # Set default storage format to "rds"

# Source custom R scripts from the "R" directory
tar_source(files = "R")

# Define the list of targets for the pipeline
list(
  tar_download(name = FIPSstates, urls = make_fipsURLS(),
               paths = make_fipsPaths(file.path("Industrial model", 
                                                "INWU_task_folders", 
                                                "Data_processing",
                                                "StateFIPS"), 
                                      make_fipsURLS()),
               error = "continue"),
  tar_target(FIPSdata, compile_fips(FIPSstates), format = "file"),
  tar_target(datafp, "state_data", format = "file"),  # Load state data file
  tar_target(existingCrosswalks, "DataCrosswalks/StateDataCrosswalks", format = "file"),  # Load existing state crosswalks
  tar_target(NationalDataCrosswalks, "DataCrosswalks/NationalDataCrosswalks", format = "file"), # Load national crosswalks
  tar_target(NAICSworkup, "Industrial model/Industrial_DataSummary_By_LEE.xlsx", format = "file"), # Load NAICS summary
  tar_target(SWUDS, "Industrial model/SWUDS_records/natprod_CN_QNTY_industrial_2024-07-12.xlsx", format = "file"), # Load SWUDS records
  tar_target(SWUDS_workup, "Industrial model/SWUDS_records/From_To_Sites missing lat lng.xlsx"), # Load added lat/lon
  tar_target(SiteSelection, "Industrial model/INWU_task_folders/Site_selection/Industrial_site_list/USEPA_HIFLD_EIA_PPP_facility_v7.csv"), # load list of sites from the Site Selection group
  tar_target(QAQCstatus, "FormattedDataOutputs/DataQAQCstatus.csv", format = "file"), # Load QAQC status from previous pipeline runs
  tar_target(status, "Industrial model/INWU_task_folders/Data_processing/StatusUpdate.csv", format = "file"), #Load status from previous pipeline runs
  tar_target(dat, command = get_all_dat(datafp)),  # List all data from the state data file
  tar_target(updatedCrosswalks, 
             command = updateCrosswalks(data = dat, existingCrosswalks = existingCrosswalks),
             cue = tar_cue(mode = "always")),  # Update state crosswalks with new data
  tar_target(renamed_rawdat, command = 
               readandrename_columns(datafp, updatedCrosswalks, existingCrosswalks, data = "State") # Read and rename columns in the data
  ),
  tar_target(reformatted_data, command = 
               reformat_data(renamed_rawdat, updatedCrosswalks, existingCrosswalks) # Reformat the data based on crosswalks
  ),
  tar_target(NonSWUDSdata, command = merge_formatteddata(reformatted_data,
                                                         updatedCrosswalks,
                                                         existingCrosswalks,
                                                         data = "State")),
  tar_target(combined_dat, command = 
               merge_nationaldata(nonSWUDS = NonSWUDSdata,
                                  natData = FormattedNationaldata$natData_merge, 
                                  natHeaders = FormattedNationaldata$natHeaders)), # Combine state data with national data
  tar_target(FormattedNationaldata, command = prep_nationaldata(
    national_Xwalks = NationalDataCrosswalks, 
    datacodes_Xwalks = updatedCrosswalks$DataCodesCrosswalk,
    existingCrosswalks,
    natdata = list(NAICSworkup, SWUDS, SWUDS_workup),
    extradata = list(FIPSdata)
  )),
   tar_target(FormattedSiteSelectiondata, command = prep_siteselection(
    national_Xwalks = NationalDataCrosswalks, 
    datacodes_Xwalks = updatedCrosswalks$DataCodesCrosswalk,
    siteselection = list(SiteSelection)
  )),
  tar_target(augmented_data, command = 
               augment_data(
                 data = combined_dat, 
                 data_to_add = FormattedNationaldata$augmentData,
                 natHeaders = FormattedNationaldata$natHeaders
               )),
  tar_target(siteselectionmerged, 
             command = merge_siteselection(data = augmented_data, 
                                           siteselection = FormattedSiteSelectiondata,
                                           siteselectionfilename = SiteSelection)),
  tar_target(AllStates, command = write_allstates(siteselectionmerged), format = "file"),  # Write combined data for all states to a file
  tar_target(QAQCupdate, command = checkQAQCstatus(siteselectionmerged, QAQCstatus),
             cue = tar_cue(mode = "always")), # Look for any changes in number of duplicates and missing data rows introduced
  tar_target(StatusUpdate, 
             command = generate_statusupdate(siteselectionmerged, 
                                             FormattedSiteSelectiondata, 
                                             updatedCrosswalks,
                                             SWUDS,
                                             SiteSelection,
                                             status,
                                             QAQCupdate))
  )


# For debugging ----
# lapply(packages, library, character.only = TRUE)  # Load all required packages 
# tar_source(files = "utility_functions")  # Source utility functions 
# tar_load_everything()  # Load all targets into the environment
# tar_make(callr_function = NULL)  # Run the pipeline in the current R session