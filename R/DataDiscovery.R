
listSTdata <- function(stDatadir) {
  headdir <- dirname(stDatadir)
  
  stDatafp <- list.files(stDatadir, full.names = TRUE, recursive = TRUE)
  
  #test if directory
  findDirs<-sapply(stDatafp,dir.exists)
  stDatafp<-stDatafp[!findDirs]
  
  fps <- stDatafp
  filenames<-lapply(fps,basename)
  if(any(grepl(".zip|.7z", fps))) {
    tmp <- tempfile()
    purrr::map(fps[grepl(".zip|.7z", fps)], ~archive::archive_extract(.x, dir = tmp))
    zipfls <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    flnms <- unlist(stringr::str_extract(zipfls, "(?<=/).*"))
    
    filenames <- c(filenames[-grep(".zip|.7z", fps)], flnms)
    fps <- c(fps[-grep(".zip|.7z", fps)], zipfls)
  }
  
  names(fps) <- filenames
  
  fps <- purrr::map(stDatafp,
                    ~{
                      fp <- .x
                      filenames <- if(grepl("\\~\\$", fp)) {
                        name <- fp
                      } else if(
                        grepl(".csv|.txt", fp)) {
                        name <- fp
                      } else if(grepl(".xlsx|.xls", fp)) {
                        name <- paste(fp, readxl::excel_sheets(fp), sep = "$")
                      } else{
                        name <- fp
                      }
                    })
  
  dat <- gsub(headdir, "", unlist(fps))
  return(dat)
  
}

listdatadirs <- function(fp) {
  states <- state.abb
  subdirs <- subset(list.dirs(fp), stringr::str_extract(list.dirs(fp), "(?<=state_data/).*") %in% states)
  return(subdirs)
}

getdatafp <- function() {
  ## If that doesn't work, another prompt should appear for the directory of the formatted state data
  path_to_remote_tracker <- svDialogs::dlg_open(
    title = "Please select the file `NonSWUDS_Data_Input_Tracking.xlsx` from the folder `state_data`:"
  )$res
  
  
  unformattedstatedata <- dirname(path_to_remote_tracker)
  return(unformattedstatedata)
}

get_all_dat <- function(fp) {
  if(!exists("fp")) {
    fp <- getdatafp()
  }
  availdat <- listdatadirs(fp)
  unlist(purrr::map(availdat, ~listSTdata(.x)))
}

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
  
  blanksiteDescripts <- filledfile |> dplyr::filter(SiteDescriptions == 1 | 
                                                LocationInfo == 1 |
                                                MonthlyData == 1 |
                                                AnnualData == 1 |
                                                Metadata == 1) |>
    dplyr::mutate(State = stringr::str_extract(file, "(?<=/)[[:alpha:]]{2}")) |>
    dplyr::select(State, file) |>
    dplyr::mutate(IsReadMe = NA) |>
    dplyr::mutate(ValueType = NA, SourceType = NA, Category = NA, Saline = NA, 
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
           Dec = NA, Units_monthly = NA, Method_monthly = NA, Annual_reported = NA,
           Units_annual_reported = NA, Method_annual_reported = NA, DataProtected = NA)
  
  return(blanksiteDescripts)
}


get_filledcsv <- function(file) {
  read.csv(file, colClasses = "character")
}


merge_data <- function(blank, filled) {
  filledfile <- filled
  fp_classified <- filledfile |> na.omit() |> dplyr::pull(file)
  
  fp_unclassified <- blank |> dplyr::filter(!file %in% fp_classified) |> dplyr::mutate(across(everything(), ~as.character(.)))
  
  d <- dplyr::bind_rows(fp_unclassified, filledfile) |> unique()
  return(d)
}

updateCrosswalks <- function(data, existingCrosswalks) {
  existingCrosswalks_read <- map(
    list.files(existingCrosswalks, pattern = ".csv", full.names = TRUE), 
    ~get_filledcsv(.x)); names(existingCrosswalks_read) <- 
      gsub(".csv", "", list.files(existingCrosswalks, pattern = ".csv"))
    
    existingStateForms <- map(
      list.files(file.path(existingCrosswalks, "StateForms"), pattern = ".csv", 
                 full.names = TRUE), 
      ~read.table(.x, colClasses = "character", sep = ",")); names(existingStateForms) <- 
      gsub(".csv", "", list.files(file.path(existingCrosswalks, "StateForms"), 
                                  pattern = ".csv"))
    
    existingCrosswalks_read$Forms <- existingStateForms
    
    updatedCrosswalks <- existingCrosswalks_read
  
    blankDataDictionary <- generate_blankcsv(data)
    updatedDataDictionary <- merge_data(blank = blankDataDictionary, 
                                      filled = existingCrosswalks_read$DataDirectories)
    write.csv(updatedDataDictionary, 
            file.path(existingCrosswalks, "DataDirectories.csv"), 
            row.names = FALSE)
    updatedCrosswalks$DataDirectories <- updatedDataDictionary
    
    blankHeaderCrosswalk <- generate_blankHeaderCrosswalkcsv(updatedDataDictionary)
    updatedHeaderCrosswalk <- merge_data(blank = blankHeaderCrosswalk, filled = existingCrosswalks_read$HeaderCrosswalk)
    write.csv(updatedHeaderCrosswalk, 
              file.path(existingCrosswalks, "HeaderCrosswalk.csv"), 
              row.names = FALSE) 
    updatedCrosswalks$HeaderCrosswalk <- updatedHeaderCrosswalk
  
    return(updatedCrosswalks)
}