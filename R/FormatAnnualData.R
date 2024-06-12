standard_Units_annual <- function(data, filename, headers, hardcoded){
  if("Units_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
     tmp <- handle_readmes(data, filename, "Units_annual_reported", hardcoded)
    } else if(!is.character(data$Units_annual_reported)) {
      tmp <- handle_headers(data, filename, "Units_annual_reported", headers, hardcoded)
    } else if(is.character(data$Units_annual_reported)) {
      tmp <- data %>%
        mutate(Units_annual_reported = case_match(Units_annual_reported,
                                          mgd_options() ~ "mgd",
                                          .default = Units_annual_reported))
      }
  }  else {tmp <- data}
  tmp
}

standard_Method_annual <- function(data, filename, headers, hardcoded){
  if("Method_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Method_annual_reported", hardcoded)
    } else {
      tmp <- data %>% 
        mutate(Method_annual_reported = case_match(Method_annual_reported,
                                          Estimated_methods() ~ "Estimated",
                                          Reported_methods() ~ "Reported",
                                          Unknown_methods() ~ NA_character_))
    }
  } else (tmp <- data)
  tmp
}


formatannualdata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {
  annual_formatted <- renamed_rawdat %>%
    imap(., ~standard_Units_annual(.x, .y, HeaderCrosswalk, hardcodedparams)) %>%
    imap(., ~standard_Method_annual(.x, .y, HeaderCrosswalk, hardcodedparams)) %>%
    add_state()
  
  return(annual_formatted)
}
