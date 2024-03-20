### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2024-03-20
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

MNdat <- loadSTdata(statedirs[["MN"]])

## Format the data ----

MN_dat_annual <- MNdat$MN_IN_WU.xlsx$`MPARS_Index_Permits-IN` %>%
  rename(SourceType = resource_category, BASIN = watershed_name, 
         SITE_NAME = landowner, PERMIT_NUM = permit_number, County = county_name, 
         CATEGORY2 = use_category, DESCRIPTION = use_type, REG_NUM = legal_description
         ) %>%
  mutate(County = str_to_upper(County),
         SourceID = resource_type, 
         SourceName = case_when(is.na(aquifer) ~ resource_name, !is.na(aquifer) ~ aquifer),
         STATE = "MN") %>%
  pivot_longer(cols = contains("use_"), names_to = "YEAR", values_to = "AnnualUse") %>%
  mutate(
    YEAR = as.numeric(gsub("use_|_mg", "", YEAR)),
    ndays = ifelse(lubridate::leap_year(lubridate::ym(paste0(YEAR, "01"))), 366, 365),
    ANNUAL_WD_MGD = AnnualUse / ndays
    ) %>%
  select(SourceType, BASIN, SITE_NAME, PERMIT_NUM, County, CATEGORY2, DESCRIPTION, REG_NUM,
         SourceID, SourceName, STATE, YEAR, ANNUAL_WD_MGD)

MN_dat_NAICS <- MNdat$MN_IN_WU.xlsx$`MN IN SIC Task` %>%
  select(PERMIT_NUM = permit_number, FacilityID = project_name, SITE_NAME = landowner, ALIAS = agent,
         County = county_name, DESCRIPTION = use_type, CATEGORY2 = use_category,
         CATEGORY = nat_water_use_cd, NAICS.CODE = naics_cd, SIC.CODE = sic_cd,
         Latitude = Lat, Longitude = Long) %>%
  mutate(County = str_to_upper(County))

MN_dat_all <- merge(MN_dat_annual, MN_dat_NAICS, all = TRUE,
                    by = c("PERMIT_NUM", "County", "CATEGORY2", "DESCRIPTION",
                           "SITE_NAME")) %>%
  filter(!CATEGORY2 %in% c("Agricultural Irrigation", "Non-Crop Irrigation",
                           "Water Supply")) %>%
  group_by(PERMIT_NUM, County, CATEGORY2, DESCRIPTION, SITE_NAME, SourceType, 
           BASIN, REG_NUM, SourceID, SourceName, STATE) %>%
  mutate(keep = all(!is.na(ANNUAL_WD_MGD))) %>%
  filter(keep) %>%
  select(-keep) %>%
  ungroup()

# Write data ----

if(outputcsv) {write.csv(
  MN_dat_all, file = file.path(formattedstatedata, "MN_formatted.csv"), row.names = FALSE
)}