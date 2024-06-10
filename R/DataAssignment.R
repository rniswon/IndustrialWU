
generate_blankcsv <- function(x) {
  blankdat <- data.frame(
    file = x,
    SiteDescriptions = NA,
    LocationInfo = NA,
    MonthlyData = NA,
    AnnualData = NA,
    Metadata = NA,
    Duplicate = NA,
    NotRelevant = NA
  )
  
  return(blankdat)
}



generate_blankHeaderCrosswalkcsv <- function(filledassignment) {
  
  filledfile <- filledassignment
  
  blanksiteDescripts <- filledfile %>% filter(SiteDescriptions == 1 | 
                                                LocationInfo == 1 |
                                                MonthlyData == 1 |
                                                AnnualData == 1 |
                                                Metadata == 1) %>%
    mutate(State = str_extract(file, "(?<=/)[[:alpha:]]{2}")) %>%
    select(State, file) %>%
    mutate(ValueType = NA, SourceType = NA, Category = NA, Saline = NA, 
           FacilityName = NA, FacilityName1 = NA, FacilityName2 = NA,
           FacilityNumber = NA, FacilityNumber1 = NA, FacilityNumber2 = NA,
           SourceName = NA, SourceName1 = NA, SourceName2 = NA,
           SourceNumber = NA, SourceNumber1 = NA, SourceNumber2 = NA, 
           NAICS = NA, SIC = NA, Description = NA, HUC8 = NA, HUC10 = NA, 
           HUC12 = NA, AquiferName1 = NA, AquiferName2 = NA,
           BasinName1 = NA, BasinName2 = NA, Address1 = NA, City1 = NA,
           County1 = NA, State1 = NA, Zip1 = NA, Address2 = NA, City2 = NA,
           County2 = NA, State2 = NA, Zip2 = NA, Lat = NA, Lon = NA, Datum = NA, 
           Projection = NA, Year = NA, Jan = NA, Feb = NA, Mar = NA, Apr = NA,
           May = NA, Jun = NA, Jul = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA,
           Dec = NA, Units_monthly = NA, Method_monthy = NA, Annual_reported = NA,
           Units_annual_reported = NA, Method_annual_reported = NA, DataProtected = NA)
  
  return(blanksiteDescripts)
}


get_filledcsv <- function(file) {
  read.csv(file, colClasses = "character")
}

merge_data <- function(blank, filled) {
  filledfile <- get_filledcsv(filled)
  fp_classified <- filledfile %>% na.omit() %>% dplyr::pull(file)
  
  fp_unclassified <- blank %>% filter(!file %in% fp_classified)
  
  d <- bind_rows(fp_unclassified, filledfile) %>% unique()
  write.csv(d, filled, row.names = FALSE)
  return(d)
}


readandrename_columns <- function(datafp, HeaderCrosswalk) {
  filledheader <- HeaderCrosswalk
  
  headers_classified <- filledheader %>% na.omit()
  
  dat <- imap(headers_classified$file, ~{
    i <- .y
    dat_raw <- read_in_datafile(datafp, .x)
    headercrosswalk <- headers_classified %>%
      slice(i) %>% select(where(~{. != ""})) %>% 
      pivot_longer(cols = -c(State, file), names_to = "NewName", 
                   values_to = "OldName")
    map2_dfc(headercrosswalk$NewName, headercrosswalk$OldName, ~{
      tibble(!!.x := dat_raw[[.y]])
    })
  })
  names(dat) <- headers_classified$file
}


