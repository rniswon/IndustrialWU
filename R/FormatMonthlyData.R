standard_Units_monthly <- function(data, filename, headers, hardcoded, codescrosswalk){
  if("Units_monthly" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Units_monthly", hardcoded, codescrosswalk)
    } else if(!is.character(data$Units_monthly)) {
      tmp <- handle_headers(data, filename, "Units_monthly", headers, hardcoded, codescrosswalk)
    } else if(is.character(data$Units_monthly)) {
      tmp <- crosswalk_codes(data, filename, "Units_monthly", codescrosswalk)}
  } else (tmp <- data)
  tmp
}

standard_Methods_monthly <- function(data, filename, hardcoded, codescrosswalk){
  if("Method_monthly" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Method_monthly", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, filename, "Method_monthly", codescrosswalk)}
  } else (tmp <- data)
  tmp
}

standard_Year <- function(data, filename, hardcoded, codescrosswalk) {
  if("Year" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Year", hardcoded, codescrosswalk)
    } else {
      tmp <- data %>% mutate(Year = as.numeric(readr::parse_number(as.character(Year))))
    }
  } else (tmp <- data)
}


formatmonthlydata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams, codescrosswalk) {

  monthly_formatted <- renamed_rawdat %>%
    imap(., ~standard_Units_monthly(.x, .y, HeaderCrosswalk, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_Methods_monthly(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_Year(.x, .y, hardcodedparams, codescrosswalk)) %>%
    add_state()
  
  return(monthly_formatted)
}
