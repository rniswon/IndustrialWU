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

TXdat <- loadSTdata(statedirs[["TX"]])

## Format the data ----

TX_WUdat <- TXdat$TX_IN_WU.xlsx$Texas_IndustrialWithdrawals_200 %>%
  mutate(ValueType = case_match(`Purchased / Self-Supplied`, "Pur" ~ "TR",
                                c("SS", "Ss") ~ "WD"),
         SourceType = case_when(
           `Purchased / Self-Supplied` == "Pur" ~ "PS",
           `Water Type` == "Groundwater" ~ "GW",
           `Water Type` == "Surface Water" ~ "SW"
         ),
         Category = case_match(UseType, "IND" ~ "IN", "MFG" ~ "MF",
                               "MIN" ~ "MI", "PWR" ~ "TE"),
         Saline = case_match(`Brackish / Saline`, "N" ~ FALSE, "Y" ~ TRUE),
         FacilityName = `Survey Name`, FacilityNumber = `TWDB Survey No`,
         SourceName = case_when(
           ValueType == "TR" ~ "Seller Name",
           ValueType == "WD" & SourceType == "GW" ~ WellName
         ),
         SourceNumber = TCEQWellId,
         Description = NAICSName,
         Address1 = str_extract(`Facility Address`, ".*(?=,.*, )"),
         City1 = str_extract(`Facility Address`, "(?<=, ).*(?=,)"),
         State1 = str_extract(`Facility Address`, "(?<=, )[[:upper:]]{2}"),
         Zip1 = str_extract(`Facility Address`, "(?<=, [[:upper:]]{2} )[[:digit:]]{5}"),
         County2 = `County Source`,
         State2 = "TX",
         across(contains("itude"), ~ifelse(. == 0, NA_real_, .)),
         across(contains("itude"), ~ifelse(. == 0, NA_real_, .)),
         Lat = ifelse(!is.na(`Well Latitude DD`), `Well Latitude DD`, `Ind Facility Latitude DD`),
         Lon = ifelse(!is.na(`Well Longitude DD`), `Well Longitude DD`, 
                            `Ind Facility Longitude DD`),
  ) %>%
  pivot_longer(cols = c(any_of(month.abb), `Total Intake Gallons`)) %>%
  mutate(date = suppressWarnings(as.Date(ifelse(name == "Total Intake Gallons", 
                                                ym(paste(Year, "01")),
                                                ym(paste(Year, name))))),
         ndays = ifelse(name == "Total Intake Gallons", 
                        ifelse(leap_year(date), 366, 365),
                        days_in_month(date)),
         wd_mgd = value / (1000000 * ndays),
         time_name = ifelse(name == "Total Intake Gallons",
                            "Annual_mgd_reported",
                            paste0(name, "_mgd"))) %>%
  pivot_wider(
    id_cols = c(ValueType, SourceType, Category, Saline, FacilityName, FacilityNumber,
                SourceNumber, NAICS, Description, Address1, City1, State1, Zip1, 
                County2, State2, Lat, Lon, Year), 
    names_from = time_name, values_from = wd_mgd, values_fn = sum) %>%
  mutate(Annual_mgd_calculated = case_when(
    leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                     30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                     29 * Feb_mgd) / 366,
    !leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                      30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                      28 * Feb_mgd) / 365
  ))


TX_SIC <- TXdat$TX_IN_WU.xlsx$`SIC-NAICS_lookup` %>%
  select(NAICS, SIC = SIC) %>%
  summarize(SIC = paste0(SIC, collapse = ", "), .by = NAICS)

TX_dat_all <- left_join(TX_WUdat, TX_SIC, by = "NAICS")

# Write data ----

if(outputcsv) {write.csv(
  TX_dat_all, file = file.path(formattedstatedata, "TX_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nFor Texas data, where multiple records exist for one facility,\n permit number, and source, in the same year, and where no data\n are available to distinguish the entries, entries have been\n summarized by summing withdrawal values.")
message("\nNote for future reference that 2020 NPDES data and 2010 Hoover data\n are also available for Texas.")
