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
  pivot_longer(cols = which(grepl("use_[[:digit:]]", names(.))), names_to = "Year", 
               values_to = "AnnualUse") %>%
  mutate(
    Year = as.numeric(gsub("use_|_mg", "", Year)),
    ndays = ifelse(lubridate::leap_year(lubridate::ym(paste0(Year, "01"))), 366, 365),
    Annual_mgd_reported = AnnualUse / ndays,
    ValueType = "WD",
    SourceType = case_match(resource_category,
                            "Groundwater" ~ "GW",
                            "Surface Water" ~ "SW"
    ),
    Category = case_match(use_category,
                          c("Agricultural Irrigation", "Non-Crop Irrigation") ~ "IR",
                          c("Heating/Cooling", "Special Categories", "Water Level Maintenance") ~ NA_character_,
                          "Industrial Processing" ~ "IN",
                          "Power Generation" ~ "TE",
                          "Water Supply" ~ "PS"
    ),
    Saline = NA,
    SourceNumber = case_when(!is.na(well_number) ~ as.character(well_number),
                             is.na(well_number) ~ resource_number),
    AquiferName1 = case_when(SourceType == "GW" ~ resource_name,
                             SourceType == "SW" ~ NA_character_),
    BasinName1 = case_when(SourceType == "GW" ~ NA_character_,
                           SourceType == "SW" ~ resource_name),
    State1 = "MN", DataProtected = NA
  ) %>%
  select(ValueType, SourceType, Category, Saline, 
         FacilityName = landowner, FacilityName1 = project_name,
         FacilityName2 = agent, FacilityNumber = permit_number, 
         FacilityNumber2 = legal_description,
         SourceName = installation_name, SourceNumber, Description = use_type,
         AquiferName1, AquiferName2 = aquifer,
         BasinName1, BasinName2 = watershed_name, County1 = county_name,
         State1, Year, Annual_mgd_reported) 


MN_dat_NAICS <- MNdat$MN_IN_WU.xlsx$`MN IN SIC Task` %>%
  mutate(Category = case_match(use_category,
                               c("Agricultural Irrigation", "Non-Crop Irrigation") ~ "IR",
                               c("Heating/Cooling", "Special Categories", "Water Level Maintenance") ~ NA_character_,
                               "Industrial Processing" ~ "IN",
                               "Power Generation" ~ "TE",
                               "Water Supply" ~ "PS"
  ),
  Description = paste(use_type, sic_ds, naics_ds, sep = ", ")
  ) %>%
  select(FacilityNumber = permit_number, FacilityName1 = project_name, 
         FacilityName = landowner, FacilityName2 = agent,
         County1 = county_name,
         Description, Category,
         CATEGORY = nat_water_use_cd, NAICS = naics_cd, SIC = sic_cd,
         Lat = Lat, Lon = Long)

MN_dat_all <- merge(MN_dat_annual, MN_dat_NAICS, all = TRUE,
                    by = c("FacilityName", "FacilityName1", "FacilityName2", 
                           "FacilityNumber", "County1"), 
                    suffix = c("_annual", "_naics")) %>%
  mutate(Category = case_when(
    !is.na(CATEGORY) ~ CATEGORY,
    !is.na(Category_annual) ~ Category_annual,
    !is.na(Category_naics) ~ Category_naics,
    is.na(CATEGORY) & is.na(Category_annual) & is.na(Category_naics) ~ NA_character_
  ),
  Description = paste(Description_annual, Description_naics, sep = "; ")) %>%
  select(-CATEGORY, -Category_annual, -Category_naics, -Description_annual,
         -Description_naics) %>%
  group_by(FacilityName, FacilityName1, FacilityName2,
           FacilityNumber, County1) %>%
  mutate(keep = any(!is.na(Annual_mgd_reported))) %>%
  filter(keep) %>%
  select(-keep) %>%
  ungroup() 

# Write data ----

if(outputcsv) {write.csv(
  MN_dat_all, file = file.path(formattedstatedata, "MN_formatted.csv"), row.names = FALSE
)}
