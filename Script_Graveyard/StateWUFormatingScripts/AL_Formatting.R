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

ALdat <- loadSTdata(statedirs[["AL"]])

## Format the data ----

ALdat_values <- bind_rows(
  ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`Withdrawal Data`,
  ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`Discharge Data`)

ALdat_sites <- bind_rows(ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`SW Intakes`,
                         ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`GW Wells`,
                         ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`DC Points`
) 

ALdat_NAICS <- ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`NAICS LIST` 

AL_dat_all <- merge(
  mutate(ALdat_values, 
         `Surface/Ground Water/Discharge Name` = case_when(
           `Wtr Source` %in% c("GW", "SW") ~ `Surface/Ground Water Name`,
           `Wtr Source` == "DC" ~ `Discharge Name`)),
  mutate(
    rename(ALdat_sites, 
           `Wtr Source` = `Water Source`,
           `Owner Name` = Owner, `Sub-basin` = HUC,
           `Basin Name` = `HUC Name`),
    `Surface/Ground Water/Discharge Name` = case_when(
      `Wtr Source` == "GW" ~ `Ground Water Name`,
      `Wtr Source` == "SW" ~ `Surface Water Name`,
      `Wtr Source` == "DC" ~ `Discharge Facility Name`
    )),
  by = c("Wtr Source", "Owner Name", "Sub-basin", "Basin Name", 
         "Certificate Number", "Certificate Category", "County",
         "Surface/Ground Water/Discharge Name"),
  all = TRUE) %>% 
  merge(., rename(ALdat_NAICS, `Owner Name` = OwnerName), 
        by = c("Owner Name", "Certificate Number"), all = TRUE) %>%
  mutate(ValueType = case_when(`Wtr Source` %in% c("SW", "GW") ~ "WD",
                               `Wtr Source` == "DC" ~ "RL"),
         SourceType = case_when(`Wtr Source` %in% c("SW", "GW") ~ `Wtr Source`,
                                `Wtr Source` == "DC" ~ NA_character_),
         Category = NA_character_, Saline = NA_character_, State = "AL", DataProtected = TRUE,
         Annual_mgd_calculated = case_when(
           leap_year(as.numeric(Year)) ~ (31 * (Jan + Mar + May + Jul + Aug + Oct + Dec) +
                                            30 * (Apr + Jun + Sep + Nov) +
                                            29 * Feb) / 366,
           !leap_year(as.numeric(Year)) ~ (31 * (Jan + Mar + May + Jul + Aug + Oct + Dec) +
                                             30 * (Apr + Jun + Sep + Nov) +
                                             28 * Feb) / 365
         )
         ) %>%
  select(ValueType, SourceType, Category, Saline, FacilityName = `Owner Name`, 
         FacilityNumber = `Certificate Number`, SourceName = `Surface/Ground Water/Discharge Name`,
         NAICS = `NAICS CODE`, HUC11 = `Sub-basin`, HUC8 = Basin,
         AquiferName1 = AquiferName, BasinName1 = `Basin Name`, BasinName2 = Source, County1 = County, State1 = State,
         Lat = Latitude, Lon = Longitude, Year = Year, Jan_mgd = Jan, Feb_mgd = Feb,
         Mar_mgd = Mar, Apr_mgd = Apr, May_mgd = May, Jun_mgd = Jun, Jul_mgd = Jul,
         Aug_mgd = Aug, Sep_mgd = Sep, Oct_mgd = Oct, Nov_mgd = Nov, Dec_mgd = Dec,
         Annual_mgd_reported = `Avg Daily (mgd)`, Annual_mgd_calculated, DataProtected
         ) 




# Write data ----

if(outputcsv) {write.csv(
  AL_dat_all, file = file.path(formattedstatedata, "AL_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nNote for future reference that 2015 NPDES data and 2010 Hoover data are also available for Alabama. \nDischarge data has been incorporated.")
