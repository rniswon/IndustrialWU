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

Categories <- c("Irrigation", "Public Supply", "Industrial", "Power", "Mining", 
                "Commercial", "Recreation Fish & Wildlife", "Agriculture", "Other Total")

OK_dat_annual <- OKdat$`Industrial Permit Water Use 2000 to 2019.xlsx`$`Ind. Water Use (a-f per year)` %>%
  mutate(ndays = ifelse(leap_year(ym(paste0(Year, "01"))), 366, 365),
         across(all_of(Categories),
                ~(. * 325851) / (1000000 * ndays))) %>%
  pivot_longer(all_of(Categories)) %>%
  mutate(ValueType = "WD",
         SourceType = `Water Type`,
         Category = case_match(
           name, "Agriculture" ~ "AG", "Commercial" ~ "CO", "Industrial" ~ "IN",
           "Irrigation" ~ "IR", "Mining" ~ "MI", "Other Total" ~ NA_character_,
           "Power" ~ "TE", "Public Supply" ~ "PS", "Recreation Fish & Wildlife" ~ NA_character_
         ),
         FacilityName = `Permit Holder`,
         FacilityNumber = as.character(`OWRB#`),
         County2 = County,
         Annual_mgd_reported = value,
         ) %>% 
  mutate(keep = (max(Annual_mgd_reported) > 0), 
         .by = c(ValueType, SourceType, Category, FacilityName, FacilityNumber, County2)) %>%
  filter(keep == TRUE) %>%
  select(ValueType, SourceType, Category, FacilityName, FacilityNumber, County2,
         Year, Annual_mgd_reported) 

OK_permitholders <- OKdat$`Industrial Permit Water Use 2000 to 2019.xlsx`$permit_data %>%
  mutate(FacilityNumber = `Water Right`, 
         FacilityName = Entity, 
         SourceType = `Water Type`,
         Address1 = address_1,
         City1 = str_extract(adress_2, "(\\w|\\s)+"),
         State1 = str_extract(adress_2, "[[:upper:]]{2}"),
         Zip1 = readr::parse_number(adress_2),
         State2 = "OK",
         County2 = COUNTY,
         SourceName = Source) %>%
  select(SourceType, FacilityName, FacilityNumber, SourceName, SIC, 
         Address1, City1, Zip1, State1, County2, State2)


OK_HUCS <- bind_rows(OKdat$`2015 INDUSTRIAL WATER USE FINAL.xlsx`$IND_SW_HUC,
                     OKdat$`2015 INDUSTRIAL WATER USE FINAL.xlsx`$IND_GW_HUC) %>%
  mutate(FacilityNumber1 = ID, FacilityNumber = as.character(PERMIT_NUMBER), 
         FacilityName = Permit_Holder, SourceType = Water_Type,
         County2 = case_when(!is.na(County) ~ County, is.na(County) ~ County...4),
         HUC8 = as.character(`HUC Code`),
         AquiferName1 = AQUIFER_CODE, AquiferName2 = AQUIFER_CODE_2,
         AquiferName3 = AQUIFER_CODE_3) %>%
  select(SourceType, FacilityName, FacilityNumber, FacilityNumber1,
         HUC8, AquiferName1, AquiferName2, AquiferName3, County2) %>%
  filter(!is.na(FacilityName))

OK_spatial <- st_drop_geometry(OK_sf) %>%
  unique() %>%
  mutate(SourceType = case_match(WATER, "Groundwater" ~ "GW"),
         FacilityName = ENTITY_NAM, FacilityNumber = as.character(PERMIT_NUM),
         Lat = LATITUDE,
         Lon = LONGITUDE, County2 = COUNTY, BasinName1 = STREAM_SYS,
         SourceNumber = RECORD_ID) %>%
  select(SourceType, FacilityName, FacilityNumber, SourceNumber, BasinName1,
         County2, Lat, Lon) %>%
  unique() %>%
  mutate(nsources = n(), .by = c(SourceType, FacilityName, 
                                 FacilityNumber, BasinName1, County2)) %>%
  ungroup()

OK_dat_all <- mutate(OK_dat_annual, FacilityName = gsub(",", " ", FacilityName)) %>%
  left_join(mutate(OK_permitholders, FacilityName = gsub(",", " ", FacilityName)),
            by = c("SourceType", "FacilityName", "FacilityNumber", "County2")) %>%
  left_join(OK_HUCS, by = c("SourceType", "FacilityNumber", "County2")) %>%
  rename(FacilityName = FacilityName.x, FacilityName1 = FacilityName.y) %>%
  left_join(mutate(OK_spatial, FacilityName = gsub(",", " ", FacilityName)), by = c("SourceType", "FacilityNumber", "County2"), 
            relationship = "many-to-many") %>%
  mutate(tmp = pmap(list(FacilityName.x, FacilityName.y, FacilityName1),
                    ~discard(unique(c(..1, ..2, ..3)), is.na)),
         FacilityName = map_chr(tmp, ~.x[1]),
         FacilityName1 = map_chr(tmp, ~.x[2]),
         FacilityName2 = map_chr(tmp, ~.x[3])) %>%
  select(ValueType, SourceType, Category, FacilityName, FacilityName1, 
         FacilityName2, FacilityNumber, FacilityNumber1, SourceName, SourceNumber,
         SIC, HUC8, AquiferName1, AquiferName2, AquiferName3, BasinName1,
         Address1, City1, Zip1, State1, County2, State2, Lat, Lon, Year, Annual_mgd_reported,
         nsources) %>%
  filter(Annual_mgd_reported > 0 | !is.na(Category)) %>%
  group_by(ValueType, SourceType, Category, FacilityName, FacilityName1, 
           FacilityName2, FacilityNumber, FacilityNumber1, 
           SIC, HUC8, AquiferName1, AquiferName2, AquiferName3, BasinName1,
           Address1, City1, Zip1, State1, County2, State2, Year, Annual_mgd_reported,
           nsources) %>%
  mutate(i = 1, cumsum = cumsum(i),
         Annual_mgd_reported = case_when(cumsum == 1 ~ Annual_mgd_reported,
                                         cumsum > 1 ~ 0)) %>%
  ungroup() %>%
  select(-nsources, -i, -cumsum)

# Write data ----

if(outputcsv) {write.csv(
  OK_dat_all, file = file.path(formattedstatedata, "OK_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nNote for future reference that 2010 Hoover data, and spatial data\n including polygons of water use areas are also available for Oklahoma.")
message("\nFor facilities in OK with numerous lat/lon points & SourceNumbers,\n all withdrawal is associated with the first point.\n The remaining lat/lon points have 0 withdrawal assigned to them.\n This distribution preserves the facility total when the distribution\n between sources is unknown.")
