
standard_ValueType <- function(data, fp, codescrosswalk, hardcoded) {
  if("ValueType" %in% names(data)) {
    if(detect_readme(fp)) {
      tmp <- handle_readmes(data, fp, "ValueType", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, fp, "ValueType", codescrosswalk)}
  } else (tmp <- data)
  tmp
}

standard_SourceType <- function(data, fp, codescrosswalk, hardcoded) {
  if("SourceType" %in% names(data)) {
    if(detect_readme(fp)) {
      tmp <- handle_readmes(data, fp, "SourceType", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, fp, "SourceType", codescrosswalk)}
  } else (tmp <- data)
  tmp
}

standard_SIC <- function(data, fp, codescrosswalk, hardcoded){
  if(length(grep("SIC", names(data))) > 0) {
    if(length(grep("SIC", names(data))) == 1) {
      if(detect_readme(fp)) {
        tmp <- handle_readmes(data, fp, "SIC", hardcoded, codescrosswalk)
      } else {      tmp <- data}

    } else if(length(grep("SIC", names(data))) > 1) {
      tmp <- concat_columns(data, "SIC")
    }
  } else (tmp <- data)
  tmp
}

standard_Category <- function(data, fp, codescrosswalk, hardcoded) {
  if("Category" %in% names(data)) {
    if(detect_readme(fp)) {
      tmp <- handle_readmes(data, fp, "Category", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, fp, "Category", codescrosswalk)}

  } else (tmp <- data)
  tmp
}

standard_FacilityNumber <- function(data, fp, codescrosswalk, hardcoded) {
  if("FacilityNumber" %in% names(data)) {
    if(detect_readme(fp)) {
      tmp <- handle_readmes(data, fp, "FacilityNumber", hardcoded, codescrosswalk)
    } else {    tmp <- data %>% mutate(FacilityNumber = as.character(FacilityNumber))}
  } else (tmp <- data)
}

standard_Saline <- function(data, fp, codescrosswalk, hardcoded) {
  if("Saline" %in% names(data)) {
    if(detect_readme(fp)) {
      tmp <- handle_readmes(data, fp, "Saline", hardcoded, codescrosswalk)
    } else {tmp <- crosswalk_codes(data, fp, "Saline", codescrosswalk)}
  } else (tmp <- data)
  tmp
}

formatsitedata <- function(renamed_rawdat, hardcodedparams, codescrosswalk) {

  site_formatted <- renamed_rawdat %>% 
    imap(., ~standard_ValueType(.x, .y, codescrosswalk, hardcodedparams)) %>%
    imap(., ~standard_SourceType(.x, .y, codescrosswalk, hardcodedparams)) %>%
    imap(., ~standard_Category(.x, .y, codescrosswalk, hardcodedparams)) %>%
    imap(., ~standard_Saline(.x, .y, codescrosswalk, hardcodedparams)) %>%
    imap(., ~standard_FacilityNumber(.x, .y, codescrosswalk, hardcodedparams)) %>%
    imap(., ~standard_SIC(.x, .y, codescrosswalk, hardcodedparams)) %>%
    add_state()
  
  return(site_formatted)
  
}
