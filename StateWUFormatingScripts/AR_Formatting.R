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

ARdat <- loadSTdata(statedirs[["AR"]])

## Format the data ----

AR_dat_all <- ARdat$`Industrial_2000-2016_shared.xlsx`$`Industrial_2000-2019` %>%
  unique() %>%
  select(SourceType = SOURCE_CD, SITE_NAME = FACILITY_NM, SourceID = LOCAL_DESC, 
         PERMIT_NUM = FACILITY_ID, County = COUNTY_NM, YEAR = WYEAR, Latitude = Lat_DD, 
         Longitude = Long_DD, SourceName = PRIN_AQUIFER, STATE = MSTATE, SIC_CD, SECOND_SIC_CODE,
         CATEGORY = USE_NM, CATEGORY2 = SIC_NM, Address_OFFICE = LOCATION,
         Town_OFFICE = CITY, Zip_OFFICE = ZIP, ACTION_CD, contains("ENT"), CNAME_2) %>%
  mutate(SIC.CODE = paste0(SIC_CD, ", ", SECOND_SIC_CODE),
         Units = gsub("MGD", "MG", ENTERED_UNITS)) %>%
  mutate(across(c(ENT_ANN_AMT, ENT_JAN, ENT_FEB, ENT_MAR, ENT_APR, ENT_MAY, 
                  ENT_JUN, ENT_JUL, ENT_AUG, ENT_SEP, ENT_OCT, ENT_NOV, ENT_DEC),
         ~case_when(
           Units == "ACFT" ~ . * 0.325851,
           Units == "GAL" ~ . / 1000000,
           Units == "MG" ~ .
         ))) %>%
  mutate(Units = "MG", SIC.CODE = gsub(", NA", "", SIC.CODE)) %>%
  pivot_longer(cols = c(ENT_ANN_AMT, ENT_JAN, ENT_FEB, ENT_MAR, ENT_APR, ENT_MAY, 
                        ENT_JUN, ENT_JUL, ENT_AUG, ENT_SEP, ENT_OCT, ENT_NOV, ENT_DEC)) %>%
  mutate(ndays = case_when(
    name %in% c("ENT_JAN", "ENT_MAR", "ENT_MAY", "ENT_JUL", "ENT_AUG", "ENT_OCT", "ENT_DEC") ~ 31,
    name %in% c("ENT_APR", "ENT_JUN", "ENT_SEP", "ENT_NOV") ~ 30,
    name == "ENT_ANN_AMT" ~ case_when(leap_year(YEAR) ~ 366, TRUE ~ 365),
    name == "ENT_FEB" ~ case_when(leap_year(YEAR) ~ 29, TRUE ~ 28))) %>%
  mutate(value_mgd = value / ndays) %>%
  pivot_wider(id_cols = c(SourceType, SITE_NAME, SourceID, PERMIT_NUM, County,
                          YEAR, Latitude, Longitude, SourceName, STATE,
                          CATEGORY, CATEGORY2, Address_OFFICE, Town_OFFICE,
                          Zip_OFFICE, ACTION_CD, SIC.CODE), names_from = name, values_from = value_mgd) %>%
  rename(ANNUAL_WD_MGD = ENT_ANN_AMT) %>%
  dplyr::rename_with(.cols = contains("ENT_"), ~paste0(str_to_title(gsub("ENT_", "", .)), "_mgd"))


# Write data ----

if(outputcsv) {write.csv(
  AR_dat_all, file = file.path(formattedstatedata, "AR_formatted.csv"), row.names = FALSE
)}

# Messages ----
