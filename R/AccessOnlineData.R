make_fipsURLS <- function() {
  fp <- paste0("https://www2.census.gov/geo/docs/reference/codes2020/place_by_cou/st", 
               stringr::str_pad(fedmatch::State_FIPS$FIPS, 2, "left", pad = "0"),
               "_", 
               stringr:: str_to_lower(fedmatch::State_FIPS$Abbreviation),
               "_place_by_county2020.txt")
  return(fp)
}

make_fipsPaths <- function(fp, URLS) {
  basenames <- basename(URLS)
  fps <- file.path(fp, basenames)
  return(fps)
}

compile_fips <- function(fps) {
  countyfips <- map_dfr(fps, ~read.table(.x, fill = TRUE, sep = "|", header = TRUE, 
                                        colClasses = "character")) |>
    dplyr::select(STATE, County1 = COUNTYNAME, STATEFP, COUNTYFP) |>
    dplyr::mutate(Full_CountyFP = paste0(STATEFP, COUNTYFP)) |>
    standard_Addresstreatment("FIPS", "County1") |>
    unique()
  
  write.csv(countyfips, file = file.path("Industrial model", 
                                         "INWU_task_folders", "Data_processing",
                                         "CountyFIPS.csv"),
            row.names = FALSE)

  return(paste(file.path("Industrial model", 
                   "INWU_task_folders", "Data_processing",
                   "CountyFIPS.csv")))
}
