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

WUcodes <- WIdat$`WI industrial water use records workup for SIC codes-Reed 20191101.xlsx`$`WI WU Codes` %>%
  select(-c(...9, ...10, ...11, ...12)) %>%
  setNames(., .[2,]) %>%
  filter(!is.na(`Water-Use Description`)) %>%
  slice(-1)

WI_dat_all <- WIdat$`WI industrial water use records workup for SIC codes-Reed 20191101.xlsx`$WI2015_WU_hicap_sel_prep %>%
  mutate(County1 = CTY_NAME, Lat = LAT, Lon = LONG, FacilityNumber = PROP_NO, 
         FacilityName = OWNER_NAME, SourceType = case_match(SRC_TYPE, "GW" ~ "GW",
                                                            "SW" ~ "SW", "GL" ~ NA_character_),
         SourceNumber = SRC_NO, SourceNumber1 = HICAP_NO, SourceNumber2 = WI_UNIQUE,
         HUC8 = HUC_8, HUC10 = HUC_10, HUC12 = HUC_12,
         BasinName1 = case_when(SourceType == "SW" ~ HU_12_NAME, TRUE ~ NA_character_),
         BasinName2 = case_when(SourceType == "SW" ~ HU_10_NAME, TRUE ~ NA_character_),
         BasinName3 = case_when(SourceType == "SW" ~ HU_8_NAME, TRUE ~ NA_character_),
         Category = case_match(COMP_CATEGORY,
                               "AQ" ~ "AQ", 
                               c("CO", "CO-OT", "CO-State Hospital") ~ "CO",
                               c("IN", "IN (special EG case)") ~ "IN",
                               c("IR", "IR-crop", "IR-golf", "IR-other") ~ "IR",
                               c("PS-CO", "PS-MUNI", "PS-OTM") ~ "PS",
                               "TE" ~ "TE"), 
         Description = pmap_chr(list(WU_Code1, WU_Code2, WQ_Comment, sic_ds, naics_ds),
                            ~{x <- WUcodes$`Water-Use Description`[match(..1, WUcodes$`WI Water-Use Code`)]
                            y <- WUcodes$`Water-Use Description`[match(..2, WUcodes$`WI Water-Use Code`)]
                            z <- ..3
                              paste(discard(unique(c(x, y, z, ..4, ..5)), is.na), collapse = "; ")
                              }
         ),
         Year = WU_Year,
         NAICS = pmap_chr(list(WU_Code1, WU_Code2, naics_cd),
                          ~{x <- WUcodes$`NAICS Code`[match(..1, WUcodes$`WI Water-Use Code`)]
                          y <- WUcodes$`NAICS Code`[match(..2, WUcodes$`WI Water-Use Code`)]
                          z <- ..3
                          paste(discard(unique(c(x, y, z)), is.na), collapse = ", ")
                          }
         ),
         SIC = pmap_chr(list(WU_Code1, WU_Code2, sic_cd),
                          ~{x <- WUcodes$`SIC Code`[match(..1, WUcodes$`WI Water-Use Code`)]
                          y <- WUcodes$`SIC Code`[match(..2, WUcodes$`WI Water-Use Code`)]
                          z <- ..3
                          paste(discard(unique(c(x, y, z)), is.na), collapse = ", ")
                          }
         ),
         State1 = "WI") %>%
  pivot_longer(cols = contains("_gal"), names_pattern = "(.*)_gal") %>%
  mutate(date = suppressWarnings(as.Date(ifelse(
    name == "Annual", ym(paste0(Year, "01")), ym(paste0(Year, name))))),
    ndays = ifelse(name == "Annual", ifelse(leap_year(date), 366, 365), days_in_month(date))) %>%
  mutate(value_mgd = value / (1000000 * ndays),
         name_mgd = paste0(str_sub(name, 1, 3), "_mgd"),
         ValueType = "WD") %>%
  pivot_wider(
    id_cols = c(ValueType, SourceType, Category, FacilityName, FacilityNumber, SourceNumber, 
                SourceNumber1, SourceNumber2, NAICS, SIC, Description, HUC8, HUC10, HUC12, BasinName1,
                BasinName2, BasinName3, County1, State1, Lat, Lon, Year),
    names_from = name_mgd, values_from = value_mgd
  ) %>%
  rename(Annual_mgd_reported = Ann_mgd) %>%
  mutate(Annual_mgd_calculated = case_when(
    leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                     30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                     29 * Feb_mgd) / 366,
    !leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                      30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                      28 * Feb_mgd) / 365
  ))

# Write data ----

if(outputcsv) {write.csv(
  WI_dat_all, file = file.path(formattedstatedata, "WI_formatted.csv"), row.names = FALSE
)}