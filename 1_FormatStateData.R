### ### ### ### ### ### ###
# Purpose: Source all state-specific R scripts
# Date: 2024-03-13
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Options ----

## Should intermediate csv files be written for each state?
## Note that until further notice, this MUST be set as TRUE

outputcsv <- TRUE # c(TRUE, FALSE)
compileStateData<-function(outputcsv){
# Setup ----

  optloadinstall <- function(x) {
  if(x %in% utils::installed.packages()) {
    library(x, character.only = TRUE)
  } else {
    install.packages(x)
    library(x, character.only = TRUE)
  }
}

packages <-  c("stringr", "purrr", "readxl", "svDialogs")
suppressPackageStartupMessages(suppressWarnings(invisible(lapply(packages, optloadinstall))))
source(file.path("utility_functions", "CheckNewSTfiles.R"))

# Set file paths ----

## Prompts for the filepath to the state_data folder on the command line. Paths can be copied and pasted in.
## The filepath for the formatted state data is derived from the filepath for the unformatted state data if possible
## If that doesn't work, another prompt should appear for the directory of the formatted state data
path_to_remote_tracker <- svDialogs::dlg_open(
      title = "Please select the file `NonSWUDS_Data_Input_Tracking.xlsx` from the folder `state_data`:"
    )$res


unformattedstatedata <- dirname(path_to_remote_tracker)

formattedstatedata <- file.path(gsub("GAP.*$", "", unformattedstatedata), 
                                "Industrial model", "INWU_task_folders", "Data_processing",
                                "FormattedStateData")
if(!dir.exists(formattedstatedata)) {
  formattedstatedata <- dirname(
    svDialogs::dlg_open(
      title = "Please select the file `AllStates_formatted.csv` from the folder `FormattedStateData`:"
    )$res
  )
}

processingscripts_dir <- file.path(".", "StateWUFormatingScripts")

statescripts <- list.files(processingscripts_dir, full.names = TRUE)

# Check for updates ----

## This checks Lisa and Carol's NonSWUDS_Data_Input_Tracking.xlsx
## If any changes are noted in the tracking file, the user will be prompted if they wish to continue or not.
## The thought was that if a state I'm not working on has changed, I can continue.
## If a state I am working on has changed, I may want to edit that state's script before compiling all the states.
## To check for updates, a local csv file is written to the project location.
## This csv isn't tracked with git, so everyone working on the code should get updates relative to when they have last run the script locally.

local_tracker <- checkSTupdates(
  path_to_remote_tracker = file.path(unformattedstatedata, 
                                     "NonSWUDS_Data_Input_Tracking.xlsx"),
               path_to_local_tracker = "nonSWUDStracker_local.csv")
list2env(local_tracker,envir = parent.frame())
if (!continue){
  stop("Compilation Terminated",call.=F)
}else{
# Source scripts ----

## Sources all state scripts. Each state script currently should write a file "XX_formatted.csv" to the `formattedstatedata` directory
## We can change it so the csv files don't have to be written, this is just a first cut
purrr::map(statescripts, ~{message(paste("Sourcing", .x)); source(.x, local = TRUE)})

# Compile all states ----

## Currently reads all files "XX_formatted.csv" from `formattedstatedata` and unites them
## This can be updated whenever to not rely on the written csv files
## The format of the XX_formatted.csv files will need to be identical
## Until we choose a format, this script will break here.
statefiles <- list.files(formattedstatedata, 
                         pattern = "[[:upper:]]{2}_formatted.csv", 
                         full.names = TRUE)
allstates <-purrr::map_dfr(statefiles, ~read.csv(.x, colClasses = "character"))
write.csv(allstates, file.path(formattedstatedata, "AllStates_formatted.csv"))

# Write new local nonSWUDStracker ----

## If the compilation successfully runs, a new local version of the tracking file is generated
write.csv(local_tracker$local_tracker, file = "nonSWUDStracker_local.csv", row.names = FALSE)
}
}#end func

compileStateData(outputcsv)