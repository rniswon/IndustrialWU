### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2023-12-12
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----

packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", 
              "tidyr", "future", "lubridate")
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

MAdat <- loadSTdata(statedirs[["MA"]])

# Format the Data ----

## The original sheet 4. Indust_Comm Sources Address With Lat Long.xlsx`$Sources_Indust_Comm_With_Lat_Lo has river "SourceName"s listed as SourceType "GROUND". 
## Not sure what to do with this.
locations <- with(MAdat, {
  dplyr::full_join(
    `1. Facility Name and Address.xlsx`$Sheet1 %>% 
      dplyr::rename(FacilityName = Facility, WaterUse = `Water Use`), 
    `4. Indust_Comm Sources Address With Lat Long.xlsx`$Sources_Indust_Comm_With_Lat_Lo,
    by = c("FacilityName", "WaterUse"),
    suffix = c("_SOURCE", "_OFFICE")
    )
}) %>%
  dplyr::rename(SITE_NAME = FacilityName, BASIN = `Major Basin`, CATEGORY = WaterUse) %>%
  dplyr::mutate(FacilityName = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME)),
         BASIN = gsub("^.*-", "", BASIN), 
         CATEGORY = dplyr::case_when(CATEGORY == "COMM" ~ "CO", CATEGORY == "INDUST" ~ "IN"),
         Town_SOURCE = stringr::str_to_upper(Town_SOURCE), 
         Address_SOURCE = stringr::str_to_upper(Address_SOURCE),
         SourceType = case_when(SourceType == "GROUND" ~ "GW", SourceType == "SURFACE" ~ "SW")) %>%
  select(SourceType, Category = CATEGORY, FacilityName, FacilityNumber = FacilityID, SourceName, SourceNumber = SourceID,
         BasinName1 = BASIN, Address1 = Address_OFFICE, Address2 = Address_SOURCE,
         City1 = Town_OFFICE, City2 = Town_SOURCE, Lat = Latitude, Lon = Longitude)

annualvalues <- purrr::map_dfr(
  MAdat$`2. Water Usage by Facility_2000-2018.xlsx`,
  ~{.x %>% tidyr::fill(Use, .direction = "down") %>% dplyr::filter(!is.na(YEAR))}
  ) %>%
  dplyr::rename(Category = Use, FacilityName = `Facility Name`, 
         ANNUAL_WD_MGD = `Annual Withdrawal Rate (MGD:Millions of Gallons per Day)`,
         BasinName1 = Basin, City2 = TOWN) %>%
  dplyr::mutate(Category = dplyr::case_when(Category == "Commercial" ~ "CO", 
                                            Category == "Industrial" ~ "IN"),
  ANNUAL_WD_MGD = round(ANNUAL_WD_MGD, 2),
  FacilityName = gsub("[[:punct:]]", "", gsub("-.*$", "", FacilityName))) %>%
   unique() %>%
  select(Category, FacilityName, FacilityNumber1 = PERMIT_NUM, 
         FacilityNumber2 = REG_NUM, City2, BasinName1, Year= YEAR, 
         Annual_mgd_reported = ANNUAL_WD_MGD)


formatMAdata <- function(x) {
  if(any(grepl("Monthly Totals", x))) {
    partA_raw <- as.data.frame(t(x[c(1:8, 27:35),])[-1,]) %>% dplyr::filter(!is.na(V1))
    row.names(partA_raw) <- NULL
    names(partA_raw) <- unlist(x$`Facility Withdrawal Report`[c(1:8, 27:35)], use.names = FALSE)
    
    partA <- partA_raw %>%
      dplyr::select(CATEGORY = `Water Use`, SITE_NAME = `Facility Name`, 
                    ANNUAL_WD_MGD = AverageDailyWithdrawalVolume, 
                    PERMIT_NUM = `Permit Number`, REG_NUM = `Registration Number`,
                    Town_SOURCE = Town, BASIN = Basin, YEAR = `Reporting Year`) %>%
      dplyr::mutate(
        CATEGORY = dplyr::case_when(CATEGORY == "COMM" ~ "COM", CATEGORY == "INDUST" ~ "IND"),
        ANNUAL_WD_MGD = round(as.numeric(ANNUAL_WD_MGD), 2), 
        Town_SOURCE = stringr::str_to_upper(Town_SOURCE),
        YEAR = as.numeric(YEAR),
        SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME))
      )
    
    colx <- grep("Period", x)
    rowx <- grep("Period", unlist(x[colx]))
    
    coly <- grep("Monthly Totals", x)
    rowy <- grep("Monthly Totals", unlist(x[coly]))
    
    y <- partA$YEAR
    
    if(rowx == rowy) {
      partB_names <- x[c((rowx-1):rowx), c(colx:(coly - 1))] 
      partB_names_df <- data.frame(SourceID = t(partB_names)[,2], SourceName = t(partB_names)[,1]) %>%
        dplyr::filter(!is.na(SourceName))
      row.names(partB_names_df) <- NULL
      partB_raw <- x[c((rowx):(rowx + 13)), c(colx:(coly - 1))] 
      names(partB_raw) <- unlist(partB_raw[1,], use.names = FALSE)

      partB <- partB_raw %>% 
        dplyr::filter(!Period %in% c("Period", "Total")) %>%
        dplyr::mutate(., across(.cols = -Period, ~as.numeric(.))) %>%
        tidyr::pivot_longer(-Period, names_to = "SourceID") %>%
        dplyr::mutate(ndays = lubridate::days_in_month(lubridate::ym(paste0(y, Period)))) %>%
        mutate(value_mgd = value / ndays,
               Period = str_sub(Period, 1, 3)) %>%
        tidyr::pivot_wider(id_cols = "SourceID", names_from = "Period", 
                    values_from = "value_mgd", names_glue = "{Period}_mgd") %>%
        dplyr::full_join(., partB_names_df, by = "SourceID")
      
    } else if(rowx != rowy) {stop("Unexpected Formatting")}
    
    partsAB <- dplyr::bind_cols(partA, partB)
    
    return(partsAB)
  }
}

monthlybysource2018 <- purrr::map_dfr(
  MAdat$`3.2 Water Withrawal by Sources_2018.xlsx`,
  ~{
    x <- .x
    formatMAdata(x)
  }
)

monthlybysource2017 <- purrr::map_dfr(
  MAdat$`3.1 Water Withdrawal by Sources_2017.xlsx`,
  ~{
    x <- .x
    formatMAdata(x)
  }
)

monthlybysource <- dplyr::bind_rows(monthlybysource2017, monthlybysource2018) %>%
  dplyr::mutate(across(c(REG_NUM, PERMIT_NUM), 
                ~dplyr::case_when(. == "N/A" ~ NA_character_, TRUE ~ .)),
                Category = case_when(
                  CATEGORY == "IND" ~ "IN", CATEGORY == "COM" ~ "CO"
                )) %>%
  select(Category, FacilityName = SITE_NAME, Annual_mgd_reported = ANNUAL_WD_MGD,
         FacilityNumber1 = PERMIT_NUM, FacilityNumber2 = REG_NUM, City2 = Town_SOURCE,
         BasinName1 = BASIN, Year = YEAR, SourceName, SourceNumber = SourceID, contains("_mgd"))

convert2decimal <- function(x) {
  degrees <- as.numeric(str_sub(x, 1, 2))
  minutes <- as.numeric(str_sub(x, 3, 4))
  seconds <- as.numeric(str_sub(x, 5, 6))
  
  degrees + (minutes + (seconds / 60)) / 60
}

MA_dat_all <- merge(
  annualvalues, monthlybysource, 
      by = c("FacilityName", "FacilityNumber1", "FacilityNumber2", "City2", "Year"), 
  all = TRUE, suffixes = c("_ANNREP", "_MONREP")) %>%
  dplyr::mutate(Category = 
           dplyr::case_when(!is.na(Category_ANNREP) ~ Category_ANNREP, 
                     TRUE ~ Category_MONREP),
         BasinName1 = 
           dplyr::case_when(!is.na(BasinName1_ANNREP) ~ BasinName1_ANNREP, 
                     TRUE ~ BasinName1_MONREP)) %>%
  dplyr::select(-c(Category_ANNREP, Category_MONREP, BasinName1_ANNREP, 
                   BasinName1_MONREP)) %>%
  dplyr::group_by(FacilityNumber1, FacilityNumber2, City2) %>%
  tidyr::fill(c(Category, BasinName1), .direction = "downup") %>%
  merge(., locations, by = c("FacilityName", "BasinName1", "Category", "City2"), 
        all = TRUE, suffix = c("_MONREP", "_LOC")) %>%
  dplyr::mutate(SourceNumber = dplyr::case_when(
    SourceNumber_MONREP == SourceNumber_LOC ~ SourceNumber_MONREP,
    is.na(SourceNumber_MONREP) ~ SourceNumber_LOC,
    is.na(SourceNumber_LOC) ~ SourceNumber_MONREP,
    TRUE ~ "DROP"
  ),
  SourceName = dplyr::case_when(
    SourceName_MONREP == SourceName_LOC ~ SourceName_MONREP,
    is.na(SourceName_MONREP) ~ SourceName_LOC,
    is.na(SourceName_LOC) ~ SourceName_MONREP,
    TRUE ~ "DROP"
  )) %>% 
  dplyr::filter(SourceNumber != "DROP", SourceName != "DROP") %>%
  dplyr::select(-c(SourceNumber_MONREP, SourceNumber_LOC, SourceName_MONREP, SourceName_LOC)) %>%
  dplyr::mutate(Annual_mgd_reported = case_when(
                  !is.na(Annual_mgd_reported_MONREP) ~ Annual_mgd_reported_MONREP,
                  is.na(Annual_mgd_reported_MONREP) ~ Annual_mgd_reported_ANNREP
                    )) %>%
  dplyr::select(-Annual_mgd_reported_MONREP, -Annual_mgd_reported_ANNREP) %>%
  mutate(State1 = "MA", State2 = "MA", ValueType = "WD", Saline = NA,
         Lat = convert2decimal(gsub("^0", "", Lat)), 
         Lon = -convert2decimal(gsub("^0", "", Lon)), DataProtected = NA,
         Annual_mgd_calculated = case_when(
           leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                            30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                            29 * Feb_mgd) / 366,
           !leap_year(as.numeric(Year)) ~ (31 * (Jan_mgd + Mar_mgd + May_mgd + Jul_mgd + Aug_mgd + Oct_mgd + Dec_mgd) +
                                             30 * (Apr_mgd + Jun_mgd + Sep_mgd + Nov_mgd) +
                                             28 * Feb_mgd) / 365
         ),
         Address1 = case_when(!is.na(Address1) ~ Address1,
                              is.na(Address1) ~ Address2),
         City1 = case_when(!is.na(City1) ~ City1,
                           is.na(City1) ~ City2)) %>% 
  select(ValueType, SourceType, Category, Saline, FacilityName, FacilityNumber,
         FacilityNumber1, FacilityNumber2, SourceName, SourceNumber,
         BasinName1, Address1, City1, State1, Address2, City2, State2, Lat, Lon,
         Year, contains("_mgd"), DataProtected) 


# MA_dat_all %>% dplyr::group_by(PERMIT_NUM, REG_NUM, YEAR, SourceID) %>% 
#   dplyr::summarize(n = dplyr::n(), .groups = "drop") %>% dplyr::filter(n > 1)

# Write data ----
if(outputcsv) {write.csv(
  MA_dat_all, file = file.path(formattedstatedata, "MA_formatted.csv"), row.names = FALSE
)}

# Messages ----
message("\nProvided lat/lon data for MA includes errors.")
