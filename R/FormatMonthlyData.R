standard_Units_monthly <- function(data, filename, headers, hardcoded){
  if("Units_monthly" %in% names(data)) {
    
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Units_monthly", hardcoded)
    } else if(!is.character(data$Units_monthly)) {
      tmp <- handle_headers(data, filename, "Units_monthly", headers, hardcoded)
    } else if(is.character(data$Units_monthly)) {
      tmp <- data %>%
        mutate(Units_monthly = case_match(Units_monthly,
                                          mgd_options() ~ "mgd",
                                          .default = Units_monthly))
      }
  } else (tmp <- data)
  tmp
}

standard_Methods_monthly <- function(data, filename, headers, hardcoded){
  if("Method_monthly" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Method_monthly", hardcoded)
    } else {
      tmp <- data %>% 
        mutate(Method_monthly = case_match(Method_monthly,
                                           Estimated_methods() ~ "Estimated",
                                           Reported_methods() ~ "Reported",
                                           Unknown_methods() ~ NA_character_,
                                           .default = Method_monthly))
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
