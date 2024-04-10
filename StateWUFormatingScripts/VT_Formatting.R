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

VTdat <- loadSTdata(statedirs[["VT"]])


## Format the data ----
VT_dat_all <- VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly` %>%
  rename_with(.fn = ~paste(VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly`[2, ], 
                            VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly`[3,])) %>%
  rename_with(.fn = ~gsub("NA ", "", .)) %>%
  mutate(Latitude = parse_number(Latitude, na = "Latitude"), 
         Longitude = parse_number(Longitude, na = "Longitude")) %>%
  filter(!is.na(Latitude)) %>%
  pivot_longer(cols = contains("Gallons per Year")) %>%
  mutate(
    PERMIT_NUM = VTWSID, SITE_NAME = `System Name`,
    Address_SOURCE = str_extract(Address, "(\\s|\\w)+"),
    Town_SOURCE = str_extract(Address, "(?<=, )\\w+"),
    STATE = "VT", CATEGORY = `USGS Water Use Code`, CATEGORY2 = Use,
    YEAR = parse_number(name),
    value_gal = parse_number(value, na = c("n/a", "not reported yet", "closed")),
    ndays = ifelse(leap_year(ym(paste(YEAR, "01"))), 366, 365),
    ANNUAL_WD_MGD = value_gal / (1000000 * ndays)
  ) %>%
  select(PERMIT_NUM, SITE_NAME, Address_SOURCE, Town_SOURCE, STATE, CATEGORY,
         CATEGORY2, YEAR, ANNUAL_WD_MGD)

#Write data ----
  
  if(outputcsv) {write.csv(
    VT_dat_all, file = file.path(formattedstatedata, "VT_formatted.csv"), row.names = FALSE
  )}