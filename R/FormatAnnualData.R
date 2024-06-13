standard_Units_annual <- function(data, filename, headers, hardcoded, codescrosswalk){
  if("Units_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
     tmp <- handle_readmes(data, filename, "Units_annual_reported", hardcoded, codescrosswalk)
    } else if(!is.character(data$Units_annual_reported)) {
      tmp <- handle_headers(data, filename, "Units_annual_reported", headers, hardcoded, codescrosswalk)
    } else if(is.character(data$Units_annual_reported)) {
      tmp <- crosswalk_codes(data, filename, "Units_annual_reported", codescrosswalk)}
  }  else {tmp <- data}
  tmp
}

standard_Method_annual <- function(data, filename, hardcoded, codescrosswalk){
  if("Method_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Method_annual_reported", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, fp, "Method_annual_reported", codescrosswalk)}
  } else (tmp <- data)
  tmp
}


formatannualdata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams, codescrosswalk) {
  annual_formatted <- renamed_rawdat %>%
    imap(., ~standard_Units_annual(.x, .y, HeaderCrosswalk, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_Method_annual(.x, .y, hardcodedparams, codescrosswalk)) %>%
    add_state()
  
  return(annual_formatted)
}
