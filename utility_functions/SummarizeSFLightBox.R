### ### ### ### ### ### ###
# Purpose: Summarize SmartFabric Lightbox parcels by state
# Date: 2024-03-26
# Contact: cchamberlin@usgs.gov (Cathy Chamberlin)
### ### ### ### ### ### ###

LB_loadandstripData <- function(path, state) {
  fp <- path
  layers <- sf::st_layers(fp)
  if(!("Assessments" %in% layers$name)) {stop("Geodatabase does not have an `Assessments` layer")}
  nfeatures <- layers$features[which(layers$name == "Assessments")]
  
  future::plan(future::multisession, workers = 4)
  
  strippeddata <- future_map_dfr(c(1:ceiling(nfeatures / 100000)),
                                 ~{
                                   options(scipen = 10000000)
                                   offset <- paste(signif((.x - 1) * 100000, 7))
                                   query <- paste0("SELECT * FROM Assessments LIMIT 100000 OFFSET ", 
                                                   (.x - 1) * 100000)
                                   dattmp <- st_read(fp, "Assessments", query = query, quiet = TRUE) %>% 
                                     st_drop_geometry()
                                   dattmp
                                 },
                                 .progress = TRUE, seed = NULL) |>
    dplyr::mutate(State = state)
  
  return(strippeddata)
  
}

LB_summarizelanduse <- function(dataframe, state) {
  if(!"USE_CODE_STD_DESC_LPS" %in% names(dataframe)) {stop("Data frame does not have a column `USE_CODE_STD_DESC_LPS`")}
  as.data.frame(table(dataframe$USE_CODE_STD_DESC_LPS)) %>% dplyr::mutate(State = state)
}

LB_unzipdata <- function(inputfile, output_dir, use_cache = TRUE) {

  state <- str_extract(inputfile, "(?<=Professional_)[[:upper:]]{2}")
  if(any(grepl(state, stringr::str_extract(list.dirs(output_dir), "(?<=Professional_)[[:upper:]]{2}")))) {
    if(use_cache == TRUE) {
      fpout <- list.dirs(output_dir, full.names = TRUE)[
        which(grepl(state, stringr::str_extract(list.dirs(output_dir),  "(?<=Professional_)[[:upper:]]{2}")))]
      } else {
      archive::archive_extract(inputfile, dir = output_dir)
          fpout <- list.dirs(output_dir, full.names = TRUE)[
            which(grepl(state, stringr::str_extract(list.dirs(output_dir),  "(?<=Professional_)[[:upper:]]{2}")))]}} else {
          archive::archive_extract(inputfile, dir = output_dir)
          fpout <- list.dirs(output_dir, full.names = TRUE)[
            which(grepl(state, stringr::str_extract(list.dirs(output_dir),  "(?<=Professional_)[[:upper:]]{2}")))]
        }
  return(fpout)
}

