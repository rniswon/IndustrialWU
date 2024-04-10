### ### ### ### ### ### ###
# Purpose: Summarize SmartFabric Lightbox parcels by state
# Date: 2024-03-26
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###
library(tictoc)
tic()
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
       if(length(unzipped_fp) > 0) {
         stripped_dat <- LB_loadandstripData(unzipped_fp, .x)
       } else(stripped_dat <- data.frame())
       if(nrow(stripped_dat) > 0) {
         dat_summary <- LB_summarizelanduse(stripped_dat, .x)
       } else {dat_summary <- data.frame()}
       dat_summary
     }) %>% tidyr::pivot_wider(id_cols = Var1, names_from = State, values_from = Freq) %>%
  dplyr::rename(LU_Description = Var1) %>%
  dplyr::mutate(LU_Description = as.character(LU_Description)) %>%
  dplyr::mutate(LU_Description = dplyr::case_when(LU_Description == "<NA>" ~ "Unclassified",
                                                  LU_Description != "<NA>" ~ LU_Description)) %>%
  dplyr::mutate(across(-LU_Description, ~tidyr::replace_na(., 0))) %>%
  dplyr::arrange(LU_Description)

write.csv(USA_LULC, file = file.path(desired_outpath, "USA_LU_summaries.csv"), row.names = FALSE)
toc()