### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2024-03-20
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----


packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
              "tidyr", "future", "sf", "lubridate")
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

OKdat <- loadSTdata(statedirs[["OK"]])

OK_sf <- st_read(file.path(names(OKdat$Permitted_GW_Wells.shp)), quiet = TRUE)

## Format the data ----

OK_dat_annual <- OKdat$`Industrial Permit Water Use 2000 to 2019.xlsx`$`Ind. Water Use (a-f per year)` %>%
  select(SourceType = `Water Type`, SITE_NAME = `Permit Holder`, PERMIT_NUM = `OWRB#`,
         County, YEAR = Year, ANNUAL_WD_acfty = Industrial) %>%
  mutate(ndays = ifelse(leap_year(ym(paste0(YEAR, "01"))), 366, 365),
         ANNUAL_WD_MGD = (ANNUAL_WD_acfty * 325851) / (1000000 * ndays),
         keep = (max(ANNUAL_WD_MGD) > 0), .by = c(SourceType, SITE_NAME, PERMIT_NUM, County),
         County = str_to_upper(County),
         STATE = "OK",
         PERMIT_NUM = as.character(PERMIT_NUM)) %>%
  filter(keep) %>%
  select(-ANNUAL_WD_acfty, -ndays, -keep)

OK_permitholders <- OKdat$`Industrial Permit Water Use 2000 to 2019.xlsx`$permit_data %>%
  select(PERMIT_NUM = `Water Right`, SITE_NAME = Entity, SourceType = `Water Type`,
         County = COUNTY, SourceID = Source, CATEGORY = Purpose, Address_OFFICE = address_1,
         Town_OFFICE = adress_2, Zip_OFFICE = adress_2, SIC.CODE = SIC) %>%
  mutate(County = str_to_upper(County),
         State_OFFICE = str_extract(Town_OFFICE, "[[:upper:]]{2}"),
         Town_OFFICE = str_extract(Town_OFFICE, "(\\w|\\s)+"),
         Zip_OFFICE = readr::parse_number(Zip_OFFICE),
         PERMIT_NUM = as.character(PERMIT_NUM),
         SITE_NAME = gsub(",", " ", SITE_NAME))

OK_HUCS <- bind_rows(OKdat$`2015 INDUSTRIAL WATER USE FINAL.xlsx`$IND_SW_HUC,
                     OKdat$`2015 INDUSTRIAL WATER USE FINAL.xlsx`$IND_GW_HUC) %>%
  select(REG_NUM = ID, PERMIT_NUM = PERMIT_NUMBER, Category = PURPOSE, 
         ALIAS = Permit_Holder, SourceType = Water_Type, County = County,
         HUC8 = `HUC Code`, SourceName = AQUIFER_CODE, BASIN = BASIN_CODE) %>%
  mutate(County = str_to_upper(County),
         PERMIT_NUM = as.character(PERMIT_NUM)) %>%
  filter(!is.na(PERMIT_NUM))

OK_spatial <- st_drop_geometry(OK_sf) %>%
  select(PERMIT_NUM, Latitude = LATITUDE, Longitude = LONGITUDE, ALIAS2 = ENTITY_NAM,
         County = COUNTY, SourceType = WATER) %>%
  mutate(County = str_to_upper(County), 
         SourceType = gsub("Groundwater", "GW", SourceType))

OK_dat_all <- OK_dat_annual %>%
  left_join(., OK_permitholders, by = c("PERMIT_NUM", "County", "SourceType", "SITE_NAME")) %>%
  left_join(OK_HUCS, by = c("PERMIT_NUM", "SourceType", "County")) %>%
  left_join(., OK_spatial, by = c("PERMIT_NUM", "SourceType", "County"), relationship = "many-to-many")

# Write data ----

if(outputcsv) {write.csv(
  OK_dat_all, file = file.path(formattedstatedata, "OK_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nNote for future reference that 2010 Hoover data, and spatial data including polygons of water use areas are also available for Oklahoma.")
message("\nOklahoma lat/lon points are the first entry in the spatial data. For some sites, up to 151 lat/lon points are given for wells.")
