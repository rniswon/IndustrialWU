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
  select(SourceType = `Water Type`, SITE_NAME = `Survey Name`,
         PERMIT_NUM = `TWDB Survey No`, TCEQWellId, `Purchased / Self-Supplied`, 
         WellName, `Seller Name`, County = `County Source`, YEAR = Year,
         any_of(month.abb), `Total Intake Gallons`, contains("DD"),
         NAICS.CODE = NAICS, CATEGORY = UseType, `Facility Address`, `Brackish / Saline`) %>%
  mutate(SourceID = case_when(`Purchased / Self-Supplied` == "Pur" ~ "Purchase",
                              `Purchased / Self-Supplied` %in% c("SS", "Ss") ~ TCEQWellId),
         SourceName = case_when(`Purchased / Self-Supplied` == "Pur" ~ `Seller Name`,
                                `Purchased / Self-Supplied` %in% c("SS", "Ss") ~ WellName),
         STATE = "TX",
         Address_SOURCE = str_extract(string = `Facility Address`, pattern = ".*(?=,.*,.*$)"),
         Town_SOURCE = str_extract(string = `Facility Address`, pattern = "(?<=,).*(?=,.*$)"),
         Zip_SOURCE = str_extract(string = `Facility Address`, pattern = "[[:digit:]]{5}$"),
         suppressWarnings(across(contains("itude"), ~readr::parse_number(.))),
         across(contains("itude"), ~ifelse(. == 0, NA_real_, .)),
         Latitude = ifelse(!is.na(`Well Latitude DD`), `Well Latitude DD`, `Ind Facility Latitude DD`),
         Longitude = ifelse(!is.na(`Well Longitude DD`), `Well Longitude DD`, 
                            `Ind Facility Longitude DD`),
         Saline = ifelse(`Brackish / Saline` == "Y", TRUE, FALSE)) %>%
  pivot_longer(cols = c(any_of(month.abb), `Total Intake Gallons`)) %>%
  mutate(date = suppressWarnings(as.Date(ifelse(name == "Total Intake Gallons", 
                                        ym(paste(YEAR, "01")),
                                        ym(paste(YEAR, name))))),
         ndays = ifelse(name == "Total Intake Gallons", 
                        ifelse(leap_year(date), 366, 365),
                        days_in_month(date)),
         wd_mgd = value / (1000000 * ndays),
         time_name = ifelse(name == "Total Intake Gallons",
                            "ANNUAL_WD_MGD",
                            paste0(name, "_mgd")),
         Self_Supply = case_when(`Purchased / Self-Supplied` == "Pur" ~ FALSE, 
                              `Purchased / Self-Supplied` %in% c("SS", "Ss") ~ TRUE)) %>%
  pivot_wider(
    id_cols = c(SourceType, SITE_NAME, PERMIT_NUM, SourceID, SourceName, County, YEAR,
                Latitude, Longitude, NAICS.CODE, STATE, CATEGORY,
                Address_SOURCE, Town_SOURCE, Zip_SOURCE, Saline),
    names_from = time_name, values_from = wd_mgd, values_fn = sum
  )

TX_SIC <- TXdat$TX_IN_WU.xlsx$`SIC-NAICS_lookup` %>%
  select(NAICS.CODE = NAICS, SIC.CODE = SIC) %>%
  summarize(SIC.CODE = paste0(SIC.CODE, collapse = ", "), .by = NAICS.CODE)

TX_dat_all <- left_join(TX_WUdat, TX_SIC, by = "NAICS.CODE")

# Write data ----

if(outputcsv) {write.csv(
  TX_dat_all, file = file.path(formattedstatedata, "TX_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nFor Texas data, where multiple records exist for one facility, permit number, and source, in the same year, and where no data are available to distiguish the entries, entries have been summarized by summing withdrawal values.")
message("\nNote for future reference that 2020 NPDES data and 2010 Hoover data are also available for Texas.")
