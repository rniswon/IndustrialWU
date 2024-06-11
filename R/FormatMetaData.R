standard_DataProtected <- function(data, filename){
  if("DataProtected" %in% names(data)) {
    
    if(grepl("ReadMe", filename)) {
      info <- unlist(data$DataProtected)
      true_options <- "No further distribution|expressed written approval"
      found_protectionstatus <- unique(gsub(true_options, "TRUE", na.omit(unlist(str_extract_all(info, true_options)))))
      tmp <- data %>%
        mutate(DataProtected = found_protectionstatus)
    }
  } else (tmp <- data)
  tmp
}

formatmetadata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {
  
  metadata_formatted <- renamed_rawdat %>%
    imap(., ~standard_DataProtected(.x, .y)) %>%
    add_state()
  
  return(metadata_formatted)
}
