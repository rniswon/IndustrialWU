### ### ### ### ### ### ###
# Purpose: Summarize SmartFabric Lightbox parcels by state
# Date: 2024-03-26
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----
# browser()
optloadinstall <- function(x) {
  if(x %in% utils::installed.packages()) {
    library(x, character.only = TRUE)
  } else {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}

packages <-  c("stringr", "purrr", "furrr", "sf")
suppressPackageStartupMessages(suppressWarnings(invisible(lapply(packages, optloadinstall))))
source(file.path("utility_functions", "SummarizeSFLightBox.R"))

# List zip files ----

path_to_LBdata <- dirname(svDialogs::dlg_open(
  title = "Please select the file `2023_Lightbox_Parcel_License.pdf` from the folder `WU Research External Collaboration - RESTRICTED_USE_Lightbox_SmartFabric`:"
)$res)

state_zips <- list.files(path_to_LBdata, pattern = ".zip")
states <- stringr::str_extract(state_zips, "(?<!^)[[:upper:]]{2}")

desired_outpath <- dirname(svDialogs::dlg_open(
  title = "Please select the file `TargetOutpath.txt` from the local folder used to unzip files:"
)$res)

# Summarize Land Use ----

USA_LULC <- map2_dfr(states, file.path(path_to_LBdata, state_zips),
     ~{
       message(paste(.x))
       unzipped_fp <- LB_unzipdata(.y, desired_outpath, TRUE)
       stripped_dat <- LB_loadandstripData(unzipped_fp, .x)
       dat_summary <- LB_summarizelanduse(stripped_dat, .x)
     })
