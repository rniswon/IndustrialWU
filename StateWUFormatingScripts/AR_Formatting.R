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
  select(SOURCE_CD,
         SIC_CD, SECOND_SIC_CODE,
         USE_NM,
         ACTION_CD, contains("ENT"), CNAME_2,
         FacilityName = FACILITY_NM,
         FacilityNumber = FACILITY_ID,
         FacilityNumber1 = OWNER_ID,
         FacilityNumber2 = DIVERTER_ID,
         SourceName = LOCAL_DESC,
         Description = SIC_NM,
         AquiferName1 = PRIN_AQUIFER,
         BasinName1 = STREAM_NM,
         Address1 = LOCATION,
         City1 = CITY,
         County1 = COUNTY_NM,
         State1 = MSTATE,
         Zip1 = ZIP,
         Lat = Lat_DD,
         Lon = Long_DD,
         Year = WYEAR) %>%
  mutate(SIC = paste0(SIC_CD, ", ", SECOND_SIC_CODE),
         Units = gsub("MGD", "MG", ENTERED_UNITS),
         ValueType = case_when(
           ACTION_CD == "AB" ~ NA_character_,
           ACTION_CD == "DL" ~ "TR",
           ACTION_CD == "PR" ~ NA_character_,
           ACTION_CD == "RL" ~ "RL",
           ACTION_CD == "WD" ~ "WD"
         ),
         SourceType = case_when(
           SOURCE_CD == "FW" ~ NA_character_,
           SOURCE_CD == "GW" ~ "GW",
           SOURCE_CD == "SW" ~ "SW",
           SOURCE_CD == "TW" ~ "PS"
         ),
         Category = case_when(USE_NM == "INDUSTRIAL" ~ "IN"),
         Saline = NA) %>%
  mutate(across(c(ENT_ANN_AMT, ENT_JAN, ENT_FEB, ENT_MAR, ENT_APR, ENT_MAY, 
                  ENT_JUN, ENT_JUL, ENT_AUG, ENT_SEP, ENT_OCT, ENT_NOV, ENT_DEC),
                ~case_when(
                  Units == "ACFT" ~ . * 0.325851,
                  Units == "GAL" ~ . / 1000000,
                  Units == "MG" ~ .
                ))) %>%
  mutate(Units = "MG", SIC = gsub(", NA", "", SIC)) %>%
  pivot_longer(cols = c(ENT_ANN_AMT, ENT_JAN, ENT_FEB, ENT_MAR, ENT_APR, ENT_MAY, 
                        ENT_JUN, ENT_JUL, ENT_AUG, ENT_SEP, ENT_OCT, ENT_NOV, ENT_DEC)) %>%
  mutate(ndays = case_when(
    name %in% c("ENT_JAN", "ENT_MAR", "ENT_MAY", "ENT_JUL", "ENT_AUG", "ENT_OCT", "ENT_DEC") ~ 31,
    name %in% c("ENT_APR", "ENT_JUN", "ENT_SEP", "ENT_NOV") ~ 30,
    name == "ENT_ANN_AMT" ~ case_when(leap_year(Year) ~ 366, TRUE ~ 365),
    name == "ENT_FEB" ~ case_when(leap_year(Year) ~ 29, TRUE ~ 28))) %>%
  mutate(value_mgd = value / ndays) %>%
  pivot_wider(id_cols = c(ValueType, SourceType, Category, Saline, FacilityName,
                          FacilityNumber, FacilityNumber1, FacilityNumber2,
                          SourceName, SIC, Description, AquiferName1,
                          BasinName1, Address1, City1, County1, State1, Zip1, 
                          Lat, Lon, Year), 
              names_from = name, values_from = value_mgd) %>%
  rename(Annual_mgd_reported = ENT_ANN_AMT) %>%
  dplyr::rename_with(.cols = contains("ENT_"), ~paste0(str_to_title(gsub("ENT_", "", .)), "_mgd")) %>%
  mutate(Annual_mgd_calculated = case_when(
    leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                     30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                     29 * Feb_mgd) / 366,
    !leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                      30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                      28 * Feb_mgd) / 365
  ))

# Write data ----

if(outputcsv) {write.csv(
  AR_dat_all, file = file.path(formattedstatedata, "AR_formatted.csv"), row.names = FALSE
)}

# Messages ----
