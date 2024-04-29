### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2023-12-12
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----


packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
              "tidyr", "future")
suppressPackageStartupMessages(suppressWarnings(invisible(lapply(packages, optloadinstall))))

source(file.path(".", "utility_functions", "loadSTdata.R"))

state_nms <- state.abb
future::plan(strategy = future::multisession)

# Load data ----

statedirs <- list.dirs(unformattedstatedata, full.names = TRUE)[
  list.dirs(unformattedstatedata, full.names = FALSE) %in% state_nms
]
names(statedirs) <- stringr::str_extract(statedirs, ".{2}$")

all_files <- purrr::map(statedirs, ~list.files(.x, full.names = TRUE))

AZdat <- loadSTdata(statedirs[["AZ"]])

## Format the data ----
AZ_dat_rough <- AZdat$`2000-2018_Industrial Data_Redacted.xlsx`$`Non-Turf Facilities` %>% 
  pivot_longer(cols = contains(c("-USE/PURPOSE-", "--")),
               names_to = c("WaterSource", "WaterPurpose"), names_sep = "-USE/PURPOSE-|--") %>% 
  filter(!is.na(value), !is.na(FACILITY)) %>% 
  janitor::remove_empty(which = "cols") %>% 
  bind_rows(., 
AZdat$`2000-2018_Industrial Data_Redacted.xlsx`$industrial_facilities %>% 
  pivot_longer(cols = contains(c("-USE/PURPOSE-", "--")),
               names_to = c("WaterSource", "WaterPurpose"), names_sep = "-USE/PURPOSE-|--") %>% 
  filter(!is.na(value), !is.na(FACILITY)) %>% 
  janitor::remove_empty(which = "cols")) %>%
  select(CATEGORY, DESCR, Q_BASE1.YR, FACILITY, NAME, ID, WaterSource, WaterPurpose, value) %>%
  unique()

# CAMA likely stands for computer assisted mass appraisal, and would be a property type code for tax assessment

#Turn and Non-Facilities do not seem to be applicable to this project

# Write data ----

# if(outputcsv) {write.csv(
#   AZ_dat_all, file = file.path(formattedstatedata, "AZ_formatted.csv"), row.names = FALSE
# )}

# Messages ----
message("\nArizona data do not contain units. \nOnly one industrial facility is listed, but amounts of water for 'Industrial Processing' at mining sites, power generation sites, and feedlots are available (without units)")
