standard_DataProtected <- function(data, filename, hardcoded, codescrosswalk){
  if("DataProtected" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "DataProtected", hardcoded, codescrosswalk)
    } else (tmp <- data)
  } else (tmp <- data)
  tmp
}

formatmetadata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams, codescrosswalk) {

  metadata_formatted <- renamed_rawdat %>%
    imap(., ~standard_DataProtected(.x, .y, hardcodedparams, codescrosswalk)) %>%
    map(., ~remove_empty(.x, which = c("rows", "cols"))) %>%
    add_state()
  
  return(metadata_formatted)
}
