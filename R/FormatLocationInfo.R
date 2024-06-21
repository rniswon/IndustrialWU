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

standard_Address1 <- function(data) {
  if("Address1" %in% names(data)) {
      tmp <- data %>%
        mutate(Address1 = case_when(
          grepl(", ", Address1) ~ str_extract(Address1, "^.*(?=,.*,[[:blank:]]*[[:upper:]]{2})"),
          TRUE ~ Address1)
        )
    } else {tmp <- data}
  tmp
}

standard_City1 <- function(data) {
  if("City1" %in% names(data)) {
    tmp <- data %>%
      mutate(City1 = case_when(
        grepl(", ", City1) ~ str_trim(str_extract(City1, "(?<=,).*(?=,[[:blank:]]*[[:upper:]]{2})")),
        TRUE ~ City1)
      )
  } else (tmp <- data)
  tmp
}

standard_State1 <- function(data) {
  if("State1" %in% names(data)) {
    tmp <- data %>%
      mutate(State1 = case_when(
        grepl(", ", State1) ~ str_extract(State1, "(?<=,)[[:blank:]]*[[:upper:]]{2}"),
        nchar(State1) > 2 ~ state.abb[match(State1, state.name)],
        TRUE ~ State1))
  } else (tmp <- data)
  tmp
  }

standard_Zip1 <- function(data) {
  if("Zip1" %in% names(data)) {
    tmp <- data %>%
      mutate(Zip1 = case_when(
        grepl(", ", Zip1) ~ str_trim(str_extract(Zip1, "(?<=[[:upper:]]{2})[[:blank:]]*[[:digit:]]{5}")),
        TRUE ~ as.character(Zip1)))
  } else (tmp <- data)
  tmp
}

standard_County1 <- function(data, fp, hardcoded, codescrosswalk) {
  if(length(grep("County1", names(data))) > 0) {
    if(length(grep("County1", names(data))) == 1) {
      if(detect_readme(fp)) {
        tmp <- handle_readmes(data, fp, "County1", hardcoded, codescrosswalk)  %>% 
          mutate(County1 = as.character(County1))
      } else {      tmp <- data %>% 
        mutate(County1 = as.character(County1))}
      
    } else if(length(grep("County1", names(data))) > 1) {
      tmp <- concat_columns(data, "County1") %>% 
        mutate(County1 = as.character(County1))
    }
  } else (tmp <- data)
  tmp
}

standard_Lat <- function(data) {
  if(length(grep("Lat", names(data))) > 0) {
    if(length(grep("Lat", names(data))) == 1) {
      tmp <- handle_coordinates(data, "Lat")
    } else if(length(grep("Lat", names(data))) > 1) {
      tmp <- handle_coordinates(data, "Lat") %>%
        concat_columns(., "Lat")  %>%
        mutate(Lat = gsub(",.*", "", Lat))
      
    }
  } else (tmp <- data)
  tmp
}

standard_Lon <- function(data) {
  if(length(grep("Lon", names(data))) > 0) {
    if(length(grep("Lon", names(data))) == 1) {
      tmp <- handle_coordinates(data, "Lon")
    } else if(length(grep("Lon", names(data))) > 1) {
      tmp <- handle_coordinates(data, "Lon") %>%
        concat_columns(., "Lon")  %>%
        mutate(Lon = gsub(",.*", "", Lon)) # keep only the first coordinate
      
    }
  } else (tmp <- data)
  tmp
}

standard_AquiferName1 <- function(data, fp, hardcoded, codescrosswalk) {
  if(length(grep("AquiferName1", names(data))) > 0) {
    if(length(grep("AquiferName1", names(data))) == 1) {
      if(detect_readme(fp)) {
        tmp <- handle_readmes(data, fp, "AquiferName1", hardcoded, codescrosswalk)  %>% 
          mutate(AquiferName1 = as.character(AquiferName1))
      } else {      tmp <- data %>% 
        mutate(AquiferName1 = as.character(AquiferName1))}
      
    } else if(length(grep("AquiferName1", names(data))) > 1) {
      tmp <- concat_columns(data, "AquiferName1") %>% 
        mutate(AquiferName1 = as.character(AquiferName1))
    }
  } else (tmp <- data)
  tmp
}

standard_AquiferName2 <- function(data, fp, hardcoded, codescrosswalk) {
  if(length(grep("AquiferName2", names(data))) > 0) {
    if(length(grep("AquiferName2", names(data))) == 1) {
      if(detect_readme(fp)) {
        tmp <- handle_readmes(data, fp, "AquiferName2", hardcoded, codescrosswalk)  %>% 
          mutate(AquiferName2 = as.character(AquiferName2))
      } else {      tmp <- data %>% 
        mutate(AquiferName2 = as.character(AquiferName2))}
      
    } else if(length(grep("AquiferName2", names(data))) > 1) {
      tmp <- concat_columns(data, "AquiferName2") %>% 
        mutate(AquiferName2 = as.character(AquiferName2))
    }
  } else (tmp <- data)
  tmp
}



formatlocationdata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams, codescrosswalk) {
  location_formatted <- renamed_rawdat %>%
    imap(., ~standard_HUC8(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_HUC10(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_HUC12(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_AquiferName1(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_AquiferName2(.x, .y, hardcodedparams, codescrosswalk)) %>%
    imap(., ~standard_Datum(.x, .y, hardcodedparams, codescrosswalk)) %>%
    map(., ~standard_Address1(.x)) %>%
    map(., ~standard_City1(.x)) %>%
    imap(., ~standard_County1(.x, .y, hardcodedparams, codescrosswalk)) %>%
    map(., ~standard_State1(.x)) %>%
    map(., ~standard_Zip1(.x)) %>%
    map(., ~standard_Lat(.x)) %>%
    map(., ~standard_Lon(.x)) %>%
    map(., ~remove_empty(.x, which = c("rows", "cols"))) %>%
    add_state()
  
  return(location_formatted)
}
