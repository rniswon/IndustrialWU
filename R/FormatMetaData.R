standard_DataProtected <- function(data, filename, hardcoded){
  if("DataProtected" %in% names(data)) {
    
    if(grepl("ReadMe", filename)) {
      tmp <- handle_readmes(data, filename, "DataProtected", hardcoded)
    }
  } else (tmp <- data)
  tmp
}

formatmetadata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {
  
  metadata_formatted <- renamed_rawdat %>%
    imap(., ~standard_DataProtected(.x, .y, hardcodedparams)) %>%
    add_state()
  
  return(metadata_formatted)
}
