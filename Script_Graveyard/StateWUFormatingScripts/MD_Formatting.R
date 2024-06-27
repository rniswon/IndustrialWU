### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2024-03-22
# Contact: lgormansanisaca@usgs.gov (Lily Gorman Sanisaca)
### ### ### ### ### ### ###

# Setup ----

#source all utility functions
invisible(lapply(list.files("./utility_functions/",
                  pattern = "[.]R$", full.names = T), source))

#install dependency packages
packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
              "tidyr", "future","tigris","zipcodeR","data.table")
optloadinstall2(packages)



state_nms <- state.abb
future::plan(strategy = future::multisession)

# Load data ----

#####################REMOVE###############
# path_to_remote_tracker <- svDialogs::dlg_open(
#   title = "Please select the file `NonSWUDS_Data_Input_Tracking.xlsx` from the folder `state_data`:"
# )$res
# 
# 
# unformattedstatedata <- dirname(path_to_remote_tracker)
# 
# formattedstatedata <- file.path(gsub("GAP.*$", "", unformattedstatedata),
#                                 "Industrial model", "INWU_task_folders", "Data_processing",
#                                 "FormattedStateData")
# if(!dir.exists(formattedstatedata)) {
#   formattedstatedata <- dirname(
#     svDialogs::dlg_open(
#       title = "Please select the file `AllStates_formatted.csv` from the folder `FormattedStateData`:"
#     )$res
#   )
# }
##########################################

allStatesFormatted<-read.csv(file.path(formattedstatedata, "AllStates_formatted.csv"))

statedirs <- list.dirs(unformattedstatedata, full.names = TRUE)[
  list.dirs(unformattedstatedata, full.names = FALSE) %in% state_nms
]
names(statedirs) <- stringr::str_extract(statedirs, ".{2}$")

all_files <- purrr::map(statedirs, ~list.files(.x, full.names = TRUE))

MDdat <- loadSTdata(statedirs[["MD"]])

## attempt to find data from allStatesFormatted in MDdat ----
#findCols<-compileSheets_findFormattedColumns(MDdat$`MD SWUDS IN data - Lovelace.xlsx`,state="MD")


## Format the data ----

MDdata<-MDdat$`MD SWUDS IN data - Lovelace.xlsx`$data_for_analysis

MDdata2 <- MDdata %>%
  rename(Address_OFFICE = `From Owner Address Line 1`,
         Address_SOURCE = `From Physical Address Line One`,
         ANNUAL_WD_MGD = `Annual Value`,
         County = `From County Name`,
         HUC12 = `From Hydrologic Unit Code`,
         Latitude = `From Decimal Latitude`,
         Longitude = `From Decimal Longitude`,
         NAICS.CODE = `From NAICS Code`,
         SIC.CODE = `From SIC Code`,
         SITE_NAME = `From Site Number`,
         SourceID = `From Station Name`,
         SourceName = `From Remarks`,
         SourceType = `Water Type Code`, 
         STATE = `From State Name`,
         Town_OFFICE = `From Owner City`,
         Town_SOURCE = `From Physical City`,
         YEAR = `Year`,
         BASIN = `From Hydrologic Unit Code Name`,
         CATEGORY = `Water Use Subtype Code`,
         DESCRIPTION = `From SIC Code Description`) %>%
  rename_with(.cols = starts_with(c(month.name)), ~paste0(month.abb, "_mgd")) %>%
  mutate(Saline = ifelse(`Salinity Name`=="Saline" & !is.na(`Salinity Name`),TRUE,
                         ifelse(is.na(`Salinity Name`),NA,FALSE)))

MDdata2<-MDdata2 %>%
  select(names(allStatesFormatted)[which(names(allStatesFormatted) %in% names(MDdata2))])

# Write data ----

if(outputcsv) {write.csv(
  MDdata2, file = file.path(formattedstatedata, "MD_formatted.csv"), row.names = FALSE
)}

# Messages ----
