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
VT_dat_annual <- VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly` %>%
  rename_with(.fn = ~paste(VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly`[2, ], 
                            VTdat$`2008_2019_ Vermont_Yearly_Large_GW_Withdrawal.xlsx`$`GW Withdrawal Yearly`[3,])) %>%
  rename_with(.fn = ~gsub("NA ", "", .)) %>%
  mutate(Latitude = parse_number(Latitude, na = "Latitude"), 
         Longitude = parse_number(Longitude, na = "Longitude")) %>%
  filter(!is.na(Latitude)) %>%
  pivot_longer(cols = contains("Gallons per Year")) %>%
  mutate(
    FacilityNumber = VTWSID,
    FacilityName = `System Name`,
    Address1 = str_extract(Address, "(\\s|\\w)+"),
    City1 = str_extract(Address, "(?<=, )\\w+"),
    State1 = str_extract(Address, "[[:upper:]]{2}"),
    Zip1 = str_extract(Address, "(?<=[[:upper:]]{2} )[[:digit:]]{5}"),
    Category = case_match(`USGS Water Use Code`,
                          "Aquaculture" ~ "AQ",
                          "Industrial" ~ "IN",
                          "Irrigation" ~ "IR",
                          "Mining" ~ "MI",
                          "Thermoelectric" ~ "TE"
    ), 
    Description = Use,
    Year = parse_number(name),
    value_gal = parse_number(value, na = c("n/a", "not reported yet", "closed")),
    ndays = ifelse(leap_year(ym(paste(Year, "01"))), 366, 365),
    Annual_mgd_reported = value_gal / (1000000 * ndays)
  ) %>%
  select(Lat = Latitude, Lon = Longitude, FacilityNumber, FacilityName, Address1, 
         City1, State1, Zip1, Category, Description, Year, Annual_mgd_reported)

VT_dat_monthly <- VTdat$VermontIndustrial_WU_info.xlsx$VT %>%
  mutate(ValueType = "WD",
    SourceType = case_match(Source_Type, 
                                 "Groundwater" ~ "GW", .default = NA_character_),
         Category = case_match(industrial,
                               "IN" ~ "IN", .default = NA_character_),
    Saline = NA,
         FacilityName = WSName,
         FacilityNumber = WSID,
         SourceName = NAME,
         SourceNumber = ST_ASGN_IDENT_CD,
         SourceNumber2 = Well_ID,
         NAICS = `NAICS (google searches)`,
    SIC = NA,
    Description = NA,
         HUC12 = HUC12, 
    AquiferName1 = NA,
    BasinName1 = NA,
    Address1 = NA,
         Town1 = WS_Town,
         Town2 = WR_Town,
    County1 = NA,
    State1 = "VT",
    Zip1 = NA,
         Lat = Best_Available_Lat,
         Lon = Best_Available_Long,
         Datum = NA,
         Projection = NA,
    Year = year(gwu_begin_date),
    Month = month(gwu_begin_date),
    Value = Estimated_Water_Use,
    Units_monthy = "? Laura Medalie is looking into it",
    Method_monthly = case_when(Estimated_Water_Use == Reported_System_Usage ~ "Metered",
                               Estimated_Water_Use != Reported_System_Usage ~ "Estimated")
         )
#Write data ----
  
  if(outputcsv) {write.csv(
    VT_dat_all, file = file.path(formattedstatedata, "VT_formatted.csv"), row.names = FALSE
  )}