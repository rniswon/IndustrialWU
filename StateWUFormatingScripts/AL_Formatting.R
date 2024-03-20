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

ALdat_wd <- ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`Withdrawal Data` %>%
  rename(SourceType = `Wtr Source`, BASIN = `Basin Name`, HUC11 = `Sub-basin`, 
         HUC8 = Basin, SITE_NAME = `Owner Name`, PERMIT_NUM = `Certificate Number`,
         SourceID = `Surface/Ground Water Name`, YEAR = Year) %>%
  rename_with(.cols = any_of(c(month.abb)), ~paste0(., "_mgd"))

ALdat_sites <- bind_rows(ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`SW Intakes` %>% rename(SourceID = `Surface Water Name`, SourceName = Source),
                         ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`GW Wells` %>% rename(SourceID = `Ground Water Name`, SourceName = AquiferName)) %>%
  rename(SourceType = `Water Source`, HUC11 = HUC, BASIN = `HUC Name`, PERMIT_NUM = `Certificate Number`, SITE_NAME = Owner)

ALdat_NAICS <- ALdat$`USGS Lovelace NP Data Request_20200812_Final.xlsx`$`NAICS LIST` %>%
  rename(PERMIT_NUM = `Certificate Number`, SITE_NAME = OwnerName)

ALdat_NPDES <- ALdat$AL_NPDES.xlsx$AL_NPDES[-c(1:3),]; names(ALdat_NPDES) <- unlist(ALdat$AL_NPDES.xlsx$AL_NPDES[3,], use.names = FALSE)

ALdat_contacts <- ALdat$AL_IN_WU.xlsx$AL_hoovers_2010 %>%
  mutate(across(contains("County"), ~str_to_upper(gsub(" County", "", .)))) %>%
  select(SITE_NAME = `Company Name`, ALIAS = `Doing Business As`,
         ALIAS2 = `Immediate Parent`, ALIAS3 = `Ultimate Parent`,
         Address = `Primary Address 1`, City = `Primary City`, 
         State = `Primary State`, Zip = `Primary Zip`, County = `Primary County`, website = `Web Address`,
         Description = `Line Of Business`, facilitysize_sqft = `Facility Size (sq.Ft)`,
         LOCATIONTYPE = `Location Type`, revenue2010_millionUSD = `Revenue ($ million)`,
         employeesonloc2010 = `Employees At This Location`, totalemployees2010 = `Total Employees`,
         SIC_primary = `Primary US SIC Code`, SIC_all = `All US SIC Codes`, 
         NAICS_primary = `Primary US NAICS Code`, NAICS_all = `All NAICS Codes`,
         Industry_primary = `Primary Industry`) 

AL_dat_all <- merge(ALdat_wd, ALdat_sites, all = TRUE, 
                    by = c("SourceType", "HUC11", "BASIN", "SITE_NAME", "PERMIT_NUM", 
                           "Certificate Category", "SourceID", "County")) %>%
  left_join(., ALdat_NAICS, by = c("PERMIT_NUM", "SITE_NAME")) %>%
  mutate(STATE = "AL") %>% rename(ANNUAL_WD_MGD = `Avg Daily (mgd)`) %>%
  select(-`Max Daily (mgd)`, -`Certificate Category`)

# Write data ----

if(outputcsv) {write.csv(
  AL_dat_all, file = file.path(formattedstatedata, "AL_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nNote for future reference that 2015 NPDES data and 2010 Hoover data are also available for Alabama.")