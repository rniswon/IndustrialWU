### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2024-03-20
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###


# Setup ----


packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
              "tidyr", "future", "sf", "lubridate", "readr")
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

CTdat <- loadSTdata(statedirs[["CT"]])


## Format the data ----

CT_dat_all <- CTdat$ConnecticutIndustrial_WU_info.xlsx$Sheet1 %>%
  select(-c(ModPermitID, WATER_USE_DATA_SOURCE)) %>% # ModPermitID == FULL_PERMIT_ID and WATER_USE_DATA_SOURCE == Source
  mutate(ValueType = "WD",
         SourceType = substr(F_TYPE, 1, 2),
         Category = case_match(TYPE_OF_WATER_USE,
                               "COM" ~ "CO",
                               "IND" ~ "IN",
                               .default = NA_character_),
         Saline = case_match(F_TYPE,
                             "SW-SALINE" ~ TRUE, .default = FALSE),
         FacilityName = CLIENT_NAME,
         FacilityNumber = FULL_PERMIT_ID,
         SourceName = FEATURE_NAME,
         SourceNumber = HydroID,
         City1 = TOWN,
         County1 = COUNTY,
         State1 = "CT",
         Lat = LATITUDE,
         Lon = LONGITUDE,
         Datum = NA,
         Projection = NA,
         Year = year(TSDateTime),
         Month = month.abb[month(TSDateTime)],
         MGD = case_when(
           CLIENT_NAME == "WINDHAM SAND & STONE, INC." ~ `TSValue in MGD` / 1000000,
           TRUE ~ `TSValue in MGD`
         ),
         Units_monthly = "MGD",
         Method_monthly = case_match(Source,
                                     c("CTDEEP_2021_Est", "CTDEEP_Estimated") ~ "Estimated",
                                     c("CTDEEP_Reported", "PA 02-102") ~ "Reported"),
         Annual_reported = NA
         ) %>%
  mutate(ndays = days_in_month(TSDateTime),
         tmp_mg = MGD * ndays) %>%
  group_by(FacilityName, FacilityNumber, SourceName, SourceNumber, Year) %>%
  mutate(tmpndays = sum(ndays), 
         tmp_mg_ann = sum(tmp_mg), 
         Annual_calculated = tmp_mg_ann / tmpndays,
         Units_annual_calculated = "MGD",
         DataProtected = NA) %>%
  pivot_wider(
    id_cols = c(ValueType, SourceType, Category, Saline, FacilityName, 
                FacilityNumber, SourceName, SourceNumber, City1, County1, State1,
                Lat, Lon, Datum, Projection, Year, Units_monthly, Method_monthly, 
                Annual_reported, Annual_calculated, DataProtected),
    names_from = Month,
    values_from = MGD
  )
#Write data ----

if(outputcsv) {write.csv(
  CT_dat_all, file = file.path(formattedstatedata, "CT_formatted.csv"), row.names = FALSE
)}