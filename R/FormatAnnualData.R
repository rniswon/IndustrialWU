standard_Units_annual <- function(data, filename, headers, hardcoded){
  if("Units_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
     stop("Have not developed this case yet")
      # tmp <- data 
    } else if(!is.character(data$Units_annual_reported)) {
      mgd_options <- mgd_options()
      orig_header <- headers %>% filter(file == filename) %>% pull(Units_annual_reported)
      found_units <- unique(gsub(mgd_options, "mgd", na.omit(unlist(str_extract_all(orig_header, mgd_options)))))
      if(!is.null(found_units)) {
        tmp <- data %>%
          mutate(Units_annual_reported = found_units)
      }
    } else if(is.character(data$Units_annual_reported)) {tmp <- data}
  }  else {tmp <- data}
  tmp
}

standard_Method_annual <- function(data, filename, headers, hardcoded){
  if("Method_annual_reported" %in% names(data)) {
    if(detect_readme(filename)) {
      tmp <- handle_readmes(data, filename, "Method_annual_reported", hardcoded)
    } else {
      tmp <- data %>% 
        mutate(Method_annual = case_match(Method_annual,
                                           c("CTDEEP_2021_Est", "CTDEEP_Estimated", "Estimated") ~ "Estimated",
                                           c("CTDEEP_Reported", "PA 02-102", "Reported") ~ "Reported"))
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
