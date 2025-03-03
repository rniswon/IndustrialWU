#' @title loadSTdata
#' @description loads data for a requested state (currently only loads data for .csv, .txt, .xlsx, or .xls files, including those in .zip files)
#'      Parallelization is used to speed up the data read\cr \cr
#' @param stDatafp character string path to state_data subdirectory for the required state
#' @return `dat` a list containing the data that is available, and an inventory of other files that were not loaded

loadSTdata <- function(stDatadir) {
  # browser()
  reqpackages <- c("furrr", "stringr", "archive", "readxl", "purrr")
  supreq <- function(x) {suppressWarnings(require(x, character.only = TRUE))}
  
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

  
  dat <- furrr::future_map(fps, ~{
    fp <- .x
    data <- if(grepl("\\~\\$", fp)) {
      sheets <- fp
      list("Temporary and/or corrupted file")
    } else if(
      grepl(".csv|.txt", fp)) {
      sheets <- fp
      read.csv(fp, fill = TRUE, header = FALSE, colClasses = "character")
    } else if(grepl(".xlsx|.xls", fp)) {
      sheets <- readxl::excel_sheets(fp)
      map(sheets, ~suppressWarnings(suppressMessages(readxl::read_excel(fp, sheet = .x, col_types = "text"))))
    } else{
      sheets <- fp
      list("Other database type (e.g. Word or Access)")
    }
    names(data) <- sheets
    data
  },
  .progress = TRUE)
  return(dat)
}
