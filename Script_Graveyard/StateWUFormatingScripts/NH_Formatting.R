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

convert2decimal <- function(x) {
  degrees <- as.numeric(str_sub(x, 1, 2))
  minutes <- as.numeric(str_sub(x, 3, 4))
  seconds <- as.numeric(str_sub(x, 5, 6))
  
  degrees + (minutes + (seconds / 60)) / 60
}

# Load data ----

statedirs <- list.dirs(unformattedstatedata, full.names = TRUE)[
  list.dirs(unformattedstatedata, full.names = FALSE) %in% state_nms
]
names(statedirs) <- stringr::str_extract(statedirs, ".{2}$")

all_files <- purrr::map(statedirs, ~list.files(.x, full.names = TRUE))

NHdat <- loadSTdata(statedirs[["NH"]])

## Format the data ----

SD_subtypes <- NHdat$WU_MetadataCodesandDefs.xls$`SD_SourceDestination Codes`[-c(1:20),c(1,2)] %>% 
  setNames(., c("SD_SUBTYPE", "SD_DESCRIPTION"))

NH_dat_all <- bind_rows(NHdat$`Export Industrial Withdrwals for USGS_2000-10.xlsx`$Export_Industrial_Withdrwals_fo,
                        NHdat$`Export Industrial Withdrwals for USGS_2011-19.xlsx`$Export_Industrial_Withdrwals_fo) %>%
  select(-contains("_24HR_MAX")) %>%
  unique() %>%
  left_join(., SD_subtypes, by = "SD_SUBTYPE") %>%
  mutate(
    ValueType = case_match(SD_ACTION,
                           "DL" ~ "TR",
                           "RL" ~ "RL",
                           "RT" ~ "RL",
                           "WL" ~ "WD"),
    SourceType = case_when(
      SD_TYPE == "AT" ~ NA_character_,
      SD_TYPE == "GW" ~ "GW",
      SD_TYPE == "SW" ~ "SW",
      SD_TYPE == "TW" & SD_ACTION == "DL" ~ "PS",
      SD_TYPE == "TW" & SD_ACTION == "RL" ~ NA_character_),
    Category = WU_CATEGORY_TYPE,
    Saline = NA,
    FacilityName = WU_NAME,
    FacilityName2 = FACILITY, 
    FacilityNumber = PERMIT_NPDES,
    SourceName = SD_NAME,
    SourceNumber = SD_ID,
    SIC = gsub(", NA", "", paste(SIC_CODE1, SIC_CODE2, SIC_CODE3, sep = ", ")),
    Description = SD_DESCRIPTION,
    BasinName1 = case_when(
      SD_TYPE == "SW" ~ SD_NAME,
      SD_TYPE != "SW" ~ NA_character_
    ),
    Address1 = WU_ROAD,
    City1 = WU_TOWN,
    State1 = "NH",
    Lat = convert2decimal(WU_LATITUDE),
    Lon = (convert2decimal(WU_LONGITUDE)) * -1
  ) %>%
  pivot_longer(cols = contains("_USAGE")) %>%
  mutate(name = gsub("M[[:digit:]]{2}_|_USAGE", "", name),
         ndays = days_in_month(ym(paste0(Year, name))),
         value_mgd = (value * 1000) / (1000000 * ndays),
         name_mgd = paste0(str_to_title(str_sub(name, 1, 3)), "_mgd")) %>%
  pivot_wider(id_cols = c(ValueType, SourceType, Category, Saline, FacilityName,
                          FacilityName2, FacilityNumber, SourceName, SourceNumber, 
                          SIC, Description, BasinName1, Address1, City1, State1, Lat,
                          Lon, Year),
              names_from = name_mgd, values_from = value_mgd) %>%
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
  NH_dat_all, file = file.path(formattedstatedata, "NH_formatted.csv"), row.names = FALSE
)}

# Messages ----
