#' @title loadSTdata
#' @description loads data for a requested state (currently only loads data for .csv, .txt, .xlsx, or .xls files, including those in .zip files)
#'      Parallelization is used to speed up the data read\cr \cr
#' @param stDatafp character string path to state_data subdirectory for the required state
#' @return `dat` a list containing the data that is available, and an inventory of other files that were not loaded

loadSTdata <- function(stDatadir) {
  require(furrr)
  require(stringr)
  require(archive)
  require(readxl)
  require(purrr)
  plan(multisession)
  
  stDatafp <- list.files(stDatadir, full.names = TRUE)
  
  fps <- stDatafp
  filenames <- lapply(fps, stringr::str_extract, pattern = "(?<=/[[:alpha:]]{2}/).*")
  if(any(grepl(".zip", fps))) {
    tmp <- tempfile()
    purrr::map(fps[grepl(".zip", fps)], ~archive::archive_extract(.x, dir = tmp))
    zipfls <- list.files(tmp, recursive = TRUE, full.names = TRUE)
    flnms <- unlist(str_extract(zipfls, "(?<=/).*"))
    
    filenames <- c(filenames[-grep(".zip", fps)], flnms)
    fps <- c(fps[-grep(".zip", fps)], zipfls)
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
      read.csv(fp, fill = TRUE, header = FALSE)
    } else if(grepl(".xlsx|.xls", fp)) {
      sheets <- readxl::excel_sheets(fp)
      map(sheets, ~suppressMessages(readxl::read_excel(fp, sheet = .x)))
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
