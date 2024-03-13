### ### ### ### ### ### ###
# Purpose: Source all state-specific R scripts
# Date: 2024-03-13
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Options
outputcsv <- TRUE # c(TRUE, FALSE)

# Setup

optloadinstall <- function(x) {
  if(x %in% utils::installed.packages()) {
    library(x, character.only = TRUE)
  } else {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}

packages <-  c("stringr", "purrr", "readxl")
lapply(X = packages, FUN = optloadinstall)
source(file.path("utility_functions", "CheckNewSTfiles.R"))

# Set file paths

unformattedstatedata_fp <- readline("Enter filepath to the folder `state_data`:") 
unformattedstatedata <- do.call(file.path, as.list(unlist(str_split(gsub('"', '', unformattedstatedata_fp), "\\\\|/"))))

formattedstatedata <- file.path(gsub("GAP.*$", "", unformattedstatedata), 
                                "Industrial model", "INWU_task_folders", "Data_processing",
                                "FormattedStateData")
if(!dir.exists(formattedstatedata)) {
  formattedstatedata_fp <- readline("Enter filepath to the folder `FormattedStateData`:") 
  formattedstatedata <- do.call(file.path, as.list(unlist(str_split(formattedstatedata_fp, "\\\\|/"))))
}

processingscripts_dir <- file.path(".", "StateWUFormatingScripts")

statescripts <- list.files(processingscripts_dir, full.names = TRUE)

# Check for updates

local_tracker <- checkSTupdates(path_to_remote_tracker = file.path(unformattedstatedata, "NonSWUDS_Data_Input_Tracking.xlsx"),
               path_to_local_tracker = "nonSWUDStracker_local.csv")

# Source scripts
map(statescripts, ~source(.x, local = TRUE))

# Compile all states
statefiles <- list.files(formattedstatedata, 
                         pattern = "[[:upper:]]{2}_formatted.csv", 
                         full.names = TRUE)
allstates <- map_dfr(statefiles, ~read.csv(.x))
write.csv(allstates, file.path(formattedstatedata, "AllStates_formatted.csv"))

# Write new local nonSWUDStracker

write.csv(local_tracker, file = "nonSWUDStracker_local.csv", row.names = FALSE)
