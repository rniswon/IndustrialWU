### ### ### ### ### ### ###
# Purpose: Format state industrial WU data from John Lovelace to combine with SWUDS
# Date: 2023-12-12
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

# Setup ----

packages <- c("purrr", "dplyr", "stringr", "readxl", "archive", "furrr", "tidyr")
lapply(packages, library, character.only = TRUE)

state_nms <- state.abb
plan(multisession, workers = 4)

# Load data ----

statedirs <- list.dirs(unformattedstatedata, full.names = TRUE)[
  list.dirs(unformattedstatedata, full.names = FALSE) %in% state_nms
  ]
names(statedirs) <- str_extract(statedirs, ".{2}$")

all_files <- map(statedirs, ~list.files(.x, full.names = TRUE))

all_data <- future_map(
  all_files, ~{
    # browser()
    fps <- .x
    filenames <- lapply(fps, str_extract, pattern = "(?<=/[[:alpha:]]{2}/).*")
    if(any(grepl(".zip", fps))) {
      tmp <- tempfile()
      map(fps[grepl(".zip", fps)], ~archive_extract(.x, dir = tmp))
      zipfls <- list.files(tmp, recursive = TRUE, full.names = TRUE)
      flnms <- unlist(str_extract(zipfls, "(?<=/).*"))
      
      filenames <- c(filenames[-grep(".zip", fps)], flnms)
      fps <- c(fps[-grep(".zip", fps)], zipfls)
      }
      
    dat <- map(fps, ~{
      fp <- .x
      data <- if(grepl("\\~\\$", fp)) {
        sheets <- fp
        list("Temporary and/or corrupted file")
      } else if(
        grepl(".csv|.txt", fp)) {
        sheets <- fp
        read.csv(fp, fill = TRUE, header = FALSE)
        } else if(grepl(".xlsx|.xls", fp)) {
          sheets <- excel_sheets(fp)
          map(sheets, ~suppressMessages(read_excel(fp, sheet = .x)))
        } else{
          sheets <- fp
          list("Other database type (e.g. Word or Access)")
        }
      names(data) <- sheets
      data
    })
    names(dat) <- filenames
    dat
  },
  .progress = TRUE
  )


# Format state by state ----
cleandata <- list()
## MA ----
locations <- with(all_data$MA, {
  full_join(
    `1. Facility Name and Address.xlsx`$Sheet1 %>% rename(FacilityName = Facility, WaterUse = `Water Use`), 
    `4. Indust_Comm Sources Address With Lat Long.xlsx`$Sources_Indust_Comm_With_Lat_Lo,
    by = c("FacilityName", "WaterUse"),
    suffix = c("_option1", "_option2")
    )
}) %>%
  rename(SITE_NAME = FacilityName, BASIN = `Major Basin`, CATEGORY = WaterUse) %>%
  mutate(SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME)),
         BASIN = gsub("^.*-", "", BASIN), 
         CATEGORY = case_when(CATEGORY == "COMM" ~ "COM", CATEGORY == "INDUST" ~ "IND")) 

annualvalues <- map_dfr(
  all_data$MA$`2. Water Usage by Facility_2000-2018.xlsx`,
  ~{.x %>% fill(Use, .direction = "down") %>% filter(!is.na(YEAR))}
  ) %>%
  rename(CATEGORY = Use, SITE_NAME = `Facility Name`, 
         ANNUAL_WD_MGD = `Annual Withdrawal Rate (MGD:Millions of Gallons per Day)`,
         BASIN = Basin) %>%
  mutate(CATEGORY = case_when(
    CATEGORY == "Commercial" ~ "COM",
    CATEGORY == "INDUSTRIAL" ~ "IND"
  ),
  ANNUAL_WD_MGD = round(ANNUAL_WD_MGD, 2),
  SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME))) %>%
   unique()

monthlybysource2018 <- map_dfr(
  all_data$MA$`3.2 Water Withrawal by Sources_2018.xlsx`,
  ~{
    x <- .x
    
    if(any(grepl("Monthly Totals", x))) {
      partA <- as.data.frame(t(x[c(1:8, 27:35),])[-1,]) %>% filter(!is.na(V1))
      names(partA) <- x$`Facility Withdrawal Report`[c(1:8, 27:35)]
      
      colx <- grep("Monthly Totals", x)
      rowx <- grep("Monthly Totals", unlist(x[colx]))
      
      partB <- as.data.frame(t(x[c((rowx + 1):(rowx + 13)), c(1, colx)])) %>% 
        mutate(across(.cols = everything(), ~as.numeric(.))) %>%
        filter(!is.na(V1))
      names(partB) <- x$`Facility Withdrawal Report`[c((rowx + 1):(rowx + 13))]
      
      partsAB <- bind_cols(partA, partB)
      rownames(partsAB) <- NULL
      
      partsAB
    }
  }
)

monthlybysource2017 <- map_dfr(
  all_data$MA$`3.1 Water Withdrawal by Sources_2017.xlsx`,
  ~{
    x <- .x
    if(any(grepl("Monthly Totals", x))) {
      partA <- as.data.frame(t(x[c(1:8, 27:35),])[-1,]) %>% filter(!is.na(V1))
      names(partA) <- x$`Facility Withdrawal Report`[c(1:8, 27:35)]
      
      colx <- grep("Monthly Totals", x)
      rowx <- grep("Monthly Totals", unlist(x[colx]))
      
      partB <- as.data.frame(t(x[c((rowx + 1):(rowx + 13)), c(1, colx)])) %>% 
        mutate(across(.cols = everything(), ~as.numeric(.))) %>%
        filter(!is.na(V1))
      names(partB) <- x$`Facility Withdrawal Report`[c((rowx + 1):(rowx + 13))]
      
      partsAB <- bind_cols(partA, partB)
      rownames(partsAB) <- NULL
      
      partsAB
    }
  }
)

monthlybysource <- bind_rows(monthlybysource2017, monthlybysource2018) %>%
  mutate(across(c(`Registration Number`, `Permit Number`), 
                ~case_when(. == "N/A" ~ NA_character_, TRUE ~ .))) %>%
  rename(CATEGORY = `Water Use`, SITE_NAME = `Facility Name`, 
         ANNUAL_WD_MGD = AverageDailyWithdrawalVolume, 
         PERMIT_NUM = `Permit Number`, REG_NUM = `Registration Number`,
         TOWN = Town, BASIN = Basin, YEAR = `Reporting Year`) %>%
  mutate(
    CATEGORY = case_when(CATEGORY == "COMM" ~ "COM", CATEGORY == "INDUST" ~ "IND"),
    ANNUAL_WD_MGD = round(as.numeric(ANNUAL_WD_MGD), 2), TOWN = str_to_upper(TOWN),
    YEAR = as.numeric(YEAR),
    SITE_NAME = gsub("[[:punct:]]", "", gsub("-.*$", "", SITE_NAME))
  )

MA_dat_all <- merge(
  annualvalues, monthlybysource, 
      by = c("REG_NUM", "PERMIT_NUM", "TOWN", "YEAR", "SITE_NAME"), 
  all = TRUE, suffixes = c("_ANNREP", "_MONREP")) %>%
  mutate(CATEGORY = 
           case_when(!is.na(CATEGORY_ANNREP) ~ CATEGORY_ANNREP, 
                     TRUE ~ CATEGORY_MONREP),
         BASIN = 
           case_when(!is.na(BASIN_ANNREP) ~ BASIN_ANNREP, 
                     TRUE ~ BASIN_MONREP)) %>%
  select(-c(CATEGORY_ANNREP, CATEGORY_MONREP, BASIN_ANNREP, BASIN_MONREP)) %>%
  group_by(PERMIT_NUM, REG_NUM, TOWN) %>%
  fill(c(CATEGORY, BASIN), .direction = "downup") %>%
  merge(., locations, by = c("SITE_NAME", "BASIN", "CATEGORY"), all = TRUE, suffix = c("", "_LOC"))


MA_dat_all %>% group_by(PERMIT_NUM, REG_NUM, YEAR, TOWN) %>% summarize(n = n()) %>% filter(n > 1)
## NH ----
## MD ----
## OH ----
## OK ----

# Join data ----
if(outputcsv) {write.csv(
  MA_dat_all, file = file.path(formattedstatedata, "MA_formatted.csv"), row.names = FALSE
)}
