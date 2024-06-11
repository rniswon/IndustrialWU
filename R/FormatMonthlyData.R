standard_Units_monthly <- function(data, filename, headers, hardcoded){
  if("Units_monthly" %in% names(data)) {
    
    if(grepl("ReadMe", filename)) {
      info <- unlist(data$Units_monthly)
      mgd_options <- "mgd|MGD|Mgald|Mgd|million gallons per day"
      found_units <- unique(gsub(mgd_options, "mgd", na.omit(unlist(str_extract_all(info, mgd_options)))))
      tmp <- data %>%
        mutate(Units_monthly = found_units)
    } else if(is.character(data$Units_monthly)) {tmp <- data}
  } else (tmp <- data)
  tmp
}

standard_Methods_monthly <- function(data, filename, headers, hardcoded){
  if("Method_monthly" %in% names(data)) {
    
    if(grepl("ReadMe", filename)) {
      tmp <- handle_readmes(data, filename, "Method_monthly", hardcoded)
    }
  } else (tmp <- data)
  tmp
}

standard_Year <- function(data) {
  if("Year" %in% names(data)) {
    tmp <- data %>% mutate(Year = as.numeric(Year))
  } else (tmp <- data)
}


formatmonthlydata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {
  
  monthly_formatted <- renamed_rawdat %>%
    imap(., ~standard_Units_monthly(.x, .y, HeaderCrosswalk, hardcodedparams)) %>%
    imap(., ~standard_Methods_monthly(.x, .y, HeaderCrosswalk, hardcodedparams)) %>%
    imap(., ~standard_Year(.x)) %>%
    add_state()
  
  return(monthly_formatted)
}
