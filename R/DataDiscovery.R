
getdatafp <- function() {
  ## If that doesn't work, another prompt should appear for the directory of the formatted state data
  path_to_remote_tracker <- svDialogs::dlg_open(
    title = "Please select the file `NonSWUDS_Data_Input_Tracking.xlsx` from the folder `state_data`:"
  )$res
  
  
  unformattedstatedata <- dirname(path_to_remote_tracker)
  return(unformattedstatedata)
}



listdatadirs <- function(fp) {
  states <- state.abb
  subdirs <- subset(list.dirs(fp), stringr::str_extract(list.dirs(fp), "(?<=state_data/).*") %in% states)
  return(subdirs)
}

listSTdata <- function(stDatadir) {
  reqpackages <- c("furrr", "stringr", "archive", "readxl", "purrr")
  supreq <- function(x) {suppressWarnings(require(x, character.only = TRUE))}
  headdir <- dirname(stDatadir)
  
  lapply(reqpackages, supreq)
  plan(multisession)
  
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
    flnms <- unlist(str_extract(zipfls, "(?<=/).*"))
    
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

get_all_dat <- function(fp) {
  if(!exists("fp")) {
    fp <- getdatafp()
  }
  availdat <- listdatadirs(fp)
  unlist(map(availdat, ~listSTdata(.x)))
}

read_in_datafile <- function(datafp, fp) {
    data <- if(grepl("\\~\\$", fp)) {
      list("Temporary and/or corrupted file")
    } else if(
      grepl(".csv|.txt", fp)) {
      read.csv(fp, fill = TRUE, header = FALSE)
    } else if(grepl(".xlsx|.xls", fp)) {
      workbook_fp <- str_extract(fp, ".*(?=\\$)")
      sheetnm <- str_extract(fp, "(?<=\\$).*")
      suppressWarnings(suppressMessages(
        readxl::read_excel(file.path(datafp, workbook_fp), sheet = sheetnm)))
    } else{
      list("Other database type (e.g. Word or Access)")
    }
    data
}
