
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


get_filledcsv <- function(file) {
  read.csv(file, colClasses = "character")
}

merge_data <- function(blank, filled) {

  filledfile <- get_filledcsv(filled)
  fp_classified <- filledfile %>% na.omit() %>% dplyr::pull(file)
  
  fp_unclassified <- blank %>% filter(!file %in% fp_classified)
  
  d <- bind_rows(fp_unclassified, filledfile) %>% unique()
  
  write.csv(d, "DataDirectories.csv", row.names = FALSE)
}
