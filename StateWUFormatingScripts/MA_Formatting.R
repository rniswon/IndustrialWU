### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2023-12-12
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----

packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", "tidyr")
lapply(packages, optloadinstall)
source(file.path(".", "utility_functions", "loadSTdata.R"))

state_nms <- state.abb
plan(multisession)

# Load data ----

statedirs <- list.dirs(unformattedstatedata, full.names = TRUE)[
  list.dirs(unformattedstatedata, full.names = FALSE) %in% state_nms
  ]
names(statedirs) <- str_extract(statedirs, ".{2}$")

all_files <- map(statedirs, ~list.files(.x, full.names = TRUE))

MAdat <- loadSTdata(statedirs[["MA"]])

# Format the Data ----
locations <- with(MAdat, {
  full_join(
    `1. Facility Name and Address.xlsx`$Sheet1 %>% rename(FacilityName = Facility, WaterUse = `Water Use`), 
    `4. Indust_Comm Sources Address With Lat Long.xlsx`$Sources_Indust_Comm_With_Lat_Lo,
    by = c("FacilityName", "WaterUse"),
    suffix = c("_SOURCE", "_OFFICE")
    )
}) %>%
  rename(SITE_NAME = FacilityName, BASIN = `Major Basin`, CATEGORY = WaterUse) %>%
  mutate(SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME)),
         BASIN = gsub("^.*-", "", BASIN), 
         CATEGORY = case_when(CATEGORY == "COMM" ~ "COM", CATEGORY == "INDUST" ~ "IND"),
         Town_SOURCE = str_to_upper(Town_SOURCE), Address_SOURCE = str_to_upper(Address_SOURCE)) 

annualvalues <- map_dfr(
  MAdat$`2. Water Usage by Facility_2000-2018.xlsx`,
  ~{.x %>% fill(Use, .direction = "down") %>% filter(!is.na(YEAR))}
  ) %>%
  rename(CATEGORY = Use, SITE_NAME = `Facility Name`, 
         ANNUAL_WD_MGD = `Annual Withdrawal Rate (MGD:Millions of Gallons per Day)`,
         BASIN = Basin, Town_SOURCE = TOWN) %>%
  mutate(CATEGORY = case_when(
    CATEGORY == "Commercial" ~ "COM",
    CATEGORY == "INDUSTRIAL" ~ "IND"
  ),
  ANNUAL_WD_MGD = round(ANNUAL_WD_MGD, 2),
  SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME))) %>%
   unique()


formatMAdata <- function(x) {

  if(any(grepl("Monthly Totals", x))) {
    partA <- as.data.frame(t(x[c(1:8, 27:35),])[-1,]) %>% filter(!is.na(V1))
    row.names(partA) <- NULL
    names(partA) <- x$`Facility Withdrawal Report`[c(1:8, 27:35)]
    
    colx <- grep("Period", x)
    rowx <- grep("Period", unlist(x[colx]))
    
    coly <- grep("Monthly Totals", x)
    rowy <- grep("Monthly Totals", unlist(x[coly]))
    
    if(rowx == rowy) {
      partB_names <- x[c((rowx-1):rowx), c(colx:(coly - 1))] 
      partB_names_df <- data.frame(SourceID = t(partB_names)[,2], SourceName = t(partB_names)[,1]) %>%
        filter(!is.na(SourceName))
      row.names(partB_names_df) <- NULL
      partB_raw <- x[c((rowx):(rowx + 13)), c(colx:(coly - 1))] 
      names(partB_raw) <- c(partB_raw[1,])
      
      partB <- partB_raw %>% 
        mutate(across(.cols = -Period, ~as.numeric(.))) %>%
        filter(Period != "Period") %>%
        pivot_longer(-Period, names_to = "SourceID") %>%
        pivot_wider(id_cols = "SourceID", names_from = "Period", 
                    values_from = "value", names_glue = "{Period}_mg") %>%
        full_join(., partB_names_df, by = "SourceID")
      
    } else if(rowx != rowy) {stop("Unexpected Formatting")}
    
    partsAB <- bind_cols(partA, partB)
    
    return(partsAB)
  }
}

monthlybysource2018 <- map_dfr(
  MAdat$`3.2 Water Withrawal by Sources_2018.xlsx`,
  ~{
    x <- .x
    formatMAdata(x)
  }
)

monthlybysource2017 <- future_map_dfr(
  MAdat$`3.1 Water Withdrawal by Sources_2017.xlsx`,
  ~{
    x <- .x
    formatMAdata(x)
  }
)

monthlybysource <- bind_rows(monthlybysource2017, monthlybysource2018) %>%
  mutate(across(c(`Registration Number`, `Permit Number`), 
                ~case_when(. == "N/A" ~ NA_character_, TRUE ~ .))) %>%
  rename(CATEGORY = `Water Use`, SITE_NAME = `Facility Name`, 
         ANNUAL_WD_MGD = AverageDailyWithdrawalVolume, 
         PERMIT_NUM = `Permit Number`, REG_NUM = `Registration Number`,
         Town_SOURCE = Town, BASIN = Basin, YEAR = `Reporting Year`) %>%
  mutate(
    CATEGORY = case_when(CATEGORY == "COMM" ~ "COM", CATEGORY == "INDUST" ~ "IND"),
    ANNUAL_WD_MGD = round(as.numeric(ANNUAL_WD_MGD), 2), Town_SOURCE = str_to_upper(Town_SOURCE),
    YEAR = as.numeric(YEAR),
    SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME))
  )

MA_dat_all <- merge(
  annualvalues, monthlybysource, 
      by = c("REG_NUM", "PERMIT_NUM", "Town_SOURCE", "YEAR", "SITE_NAME"), 
  all = TRUE, suffixes = c("_ANNREP", "_MONREP")) %>%
  mutate(CATEGORY = 
           case_when(!is.na(CATEGORY_ANNREP) ~ CATEGORY_ANNREP, 
                     TRUE ~ CATEGORY_MONREP),
         BASIN = 
           case_when(!is.na(BASIN_ANNREP) ~ BASIN_ANNREP, 
                     TRUE ~ BASIN_MONREP)) %>%
  select(-c(CATEGORY_ANNREP, CATEGORY_MONREP, BASIN_ANNREP, BASIN_MONREP)) %>%
  group_by(PERMIT_NUM, REG_NUM, Town_SOURCE) %>%
  fill(c(CATEGORY, BASIN), .direction = "downup") %>%
  merge(., locations, by = c("SITE_NAME", "BASIN", "CATEGORY", "Town_SOURCE"), all = TRUE, suffix = c("_MONREP", "_LOC")) %>%
  mutate(SourceID = case_when(
    SourceID_MONREP == SourceID_LOC ~ SourceID_MONREP,
    is.na(SourceID_MONREP) ~ SourceID_LOC,
    is.na(SourceID_LOC) ~ SourceID_MONREP,
    TRUE ~ "DROP"
  ),
  SourceName = case_when(
    SourceName_MONREP == SourceName_LOC ~ SourceName_MONREP,
    is.na(SourceName_MONREP) ~ SourceName_LOC,
    is.na(SourceName_LOC) ~ SourceName_MONREP,
    TRUE ~ "DROP"
  )) %>% 
  filter(SourceID != "DROP", SourceName != "DROP") %>%
  select(-c(SourceID_MONREP, SourceID_LOC, SourceName_MONREP, SourceName_LOC)) 


MA_dat_all %>% group_by(PERMIT_NUM, REG_NUM, YEAR, SourceID) %>% summarize(n = n()) %>% filter(n > 1)

# Join data ----
if(outputcsv) {write.csv(
  MA_dat_all, file = file.path(formattedstatedata, "MA_formatted.csv"), row.names = FALSE
)}
