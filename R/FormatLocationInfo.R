standard_HUC8 <- function(data, filename, hardcoded, codescrosswalk){
  if("HUC8" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "HUC8", hardcoded, codescrosswalk)
    } else {    tmp <- data %>%
      mutate(HUC8 = substr(as.character(HUC8), 1, 8))}
  } else (tmp <- data)
  tmp
}

standard_HUC10 <- function(data, filename, hardcoded, codescrosswalk){
  if("HUC10" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "HUC10", hardcoded, codescrosswalk)
    } else {    tmp <- data %>%
      mutate(HUC10 = substr(as.character(HUC10), 1, 10))}
  } else (tmp <- data)
  tmp
}

standard_HUC12 <- function(data, filename, hardcoded, codescrosswalk){
  if("HUC12" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "HUC12", hardcoded, codescrosswalk)
    } else {
      tmp <- data %>%
        mutate(HUC12 = substr(as.character(HUC12), 1, 12))
    }
  } else (tmp <- data)
  tmp
}

standard_Datum <- function(data, filename, hardcoded, codescrosswalk) {
  if("Datum" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Datum", hardcoded, codescrosswalk)
    } else {
      tmp <- data
    }
  } else (tmp <- data)
  tmp
}


formatlocationdata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams, codescrosswalk) {
  location_formatted <- renamed_rawdat %>%
    imap(., ~standard_HUC8(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_HUC10(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_HUC12(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_Datum(.x, .y, hardcodedparams, codescrosswalk)) %>%
    add_state()
  
  return(location_formatted)
}
