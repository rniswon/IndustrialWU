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

NHdat <- loadSTdata(statedirs[["NH"]])

## Format the data ----

NH_dat_all <- bind_rows(NHdat$`Export Industrial Withdrwals for USGS_2000-10.xlsx`$Export_Industrial_Withdrwals_fo,
                        NHdat$`Export Industrial Withdrwals for USGS_2011-19.xlsx`$Export_Industrial_Withdrwals_fo) %>%
  select(-contains("_24HR_MAX")) %>%
  unique() %>%
  filter(SD_ACTION %in% c("DL", "WL")) %>%
  select(SourceType = SD_TYPE, SITE_NAME = WU_NAME, PERMIT_NUM = PERMIT_NPDES,
         SourceID = SD_ID, SourceName = SD_NAME, YEAR = Year,
         contains("_USAGE"), Latitude = WU_LATITUDE, Longitude = WU_LONGITUDE,
         CATEGORY = WU_CATEGORY_TYPE, Town_SOURCE= WU_TOWN, Address_SOURCE = WU_ROAD,
         ALIAS = FACILITY, contains("SIC_CODE")) %>%
  mutate(Latitude = Latitude / 10000, Longitude = -(Longitude / 10000),
         SIC.CODE = ifelse(is.na(SIC_CODE3), 
                           ifelse(is.na(SIC_CODE2), paste(SIC_CODE1), 
                                  paste(SIC_CODE1, SIC_CODE2, sep = ", ")),
                           paste(SIC_CODE1, SIC_CODE2, SIC_CODE3, sep = ", "))) %>%
  pivot_longer(cols = contains("_USAGE")) %>%
  mutate(name = gsub("M[[:digit:]]{2}_|_USAGE", "", name),
         ndays = days_in_month(ym(paste0(YEAR, name))),
         value_mgd = (value * 1000) / (1000000 * ndays),
         name_mgd = paste0(str_to_title(str_sub(name, 1, 3)), "_mgd")) %>%
  pivot_wider(id_cols = c(SourceType, SITE_NAME, PERMIT_NUM, SourceID, SourceName,
                          YEAR, Latitude, Longitude, CATEGORY, Town_SOURCE, Address_SOURCE,
                          ALIAS, SIC.CODE),
              names_from = name_mgd, values_from = value_mgd) %>%
  mutate(STATE = "NH")

# Write data ----

if(outputcsv) {write.csv(
  NH_dat_all, file = file.path(formattedstatedata, "NH_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nNew Hampshire release and return data for facilities are also available, but are currently not included in this compilation.")