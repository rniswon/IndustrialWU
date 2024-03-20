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

WIdat <- loadSTdata(statedirs[["WI"]])

## Format the data ----

WI_dat_all <- WIdat$`WI industrial water use records workup for SIC codes-Reed 20191101.xlsx`$WI2015_WU_hicap_sel_prep %>%
  select(SourceType = SRC_TYPE, HUC8 = HUC_8, HUC10 = HUC_10, HUC12 = HUC_12,
         BASIN = HU_8_NAME, SITE_NAME = OWNER_NAME, PERMIT_NUM = WI_UNIQUE,
         County = CTY_NAME, YEAR = WU_Year, contains("gal"), NAICS.CODE = naics_cd,
         SIC.CODE = sic_cd, SourceID = SRC_NO, Latitude = LAT,
         Longitude = LONG, CATEGORY = COMP_CATEGORY, CATEGORY2 = WU_Code1) %>%
  mutate(County = str_to_upper(County), STATE = "WI") %>%
  pivot_longer(cols = contains("_gal"), names_pattern = "(.*)_gal") %>%
  mutate(date = suppressWarnings(as.Date(ifelse(
    name == "Annual", ym(paste0(YEAR, "01")), ym(paste0(YEAR, name))))),
    ndays = ifelse(name == "Annual", ifelse(leap_year(date), 366, 365), days_in_month(date))) %>%
  mutate(value_mgd = value / (1000000 * ndays),
         name_mgd = paste0(str_sub(name, 1, 3), "_mgd")) %>%
  pivot_wider(
    id_cols = c(SourceType, contains("HUC"), BASIN, SITE_NAME, PERMIT_NUM, 
                County, YEAR, NAICS.CODE, SIC.CODE, SourceID, Latitude, 
                Longitude, CATEGORY, CATEGORY2, STATE),
    names_from = name_mgd, values_from = value_mgd
  ) %>%
  rename(ANNUAL_WD_MGD = Ann_mgd)

# Write data ----

if(outputcsv) {write.csv(
  WI_dat_all, file = file.path(formattedstatedata, "WI_formatted.csv"), row.names = FALSE
)}