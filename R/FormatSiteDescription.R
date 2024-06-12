
standard_ValueType <- function(data) {
  if("ValueType" %in% names(data)) {
    tmp <- data %>%
      mutate(ValueType = case_match(ValueType,
                 c("SW", "GW", "WD") ~ "WD",
                 c("DC", "RL") ~ "RL",
                 c("DL", "PR") ~ "TR",
                 c("AB") ~ NA_character_))
  } else (tmp <- data)
  tmp
}

standard_SourceType <- function(data) {
  if("SourceType" %in% names(data)) {
    tmp <- data %>%
      mutate(SourceType = case_match(SourceType,
                                    c("SW", "SW-SALINE") ~ "SW",
                                    c("GW") ~ "GW",
                                    c("TW") ~ "PS",
                                    c("DC", "FW") ~ NA_character_))
  } else (tmp <- data)
  tmp
}

standard_SIC <- function(data){
  if(length(grep("SIC", names(data))) > 0) {
    if(length(grep("SIC", names(data))) == 1) {
      tmp <- data
    } else if(length(grep("SIC", names(data))) > 1) {
      tmp <- concat_columns(data, "SIC")
    }
  } else (tmp <- data)
  tmp
}

standard_Category <- function(data) {
  if("Category" %in% names(data)) {
    tmp <- data %>%
      mutate(Category = case_match(Category,
                                   c("AG", "FACILITY CATTLE FEEDLOT", "FACILITY DAIRY") ~ "AG",
                                   c("AQ") ~ "AQ",
                                   c("CO", "COM", "Commercial") ~ "CO",
                                   c("INDUSTRIAL", "IN", "IND", "FACILITY INDUSTRIAL USE", "Industrial") ~ "IN",
                                   c("IR") ~ "IR",
                                   c("MF") ~ "MF",
                                   c("MI", "FACILITY METAL MINING", "FACILITY SAND AND GRAVEL", "Mining") ~ "MI",
                                   c("PS", "WS", "Water Supply") ~ "PS",
                                   c("TE", "FACILITY POWER GENERATION") ~ "TE",
                                   c("RM", "Remediation") ~ "RM",
                                   c("PT", "OTH", "Total") ~ NA_character_
                                   ))
  } else (tmp <- data)
  tmp
}

standard_FacilityNumber <- function(data) {
  if("FacilityNumber" %in% names(data)) {
    tmp <- data %>% mutate(FacilityNumber = as.character(FacilityNumber))
  } else (tmp <- data)
}

standard_Saline <- function(data) {
  if("Saline" %in% names(data)) {
    tmp <- data %>%
      mutate(Saline = case_match(Saline,
                                     c("SW-SALINE") ~ TRUE,
                                 .default = FALSE))
  } else (tmp <- data)
  tmp
}

formatsitedata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {

  site_formatted <- renamed_rawdat %>% 
    map(., ~standard_ValueType(.x)) %>%
    map(., ~standard_SourceType(.x)) %>%
    map(., ~standard_Category(.x)) %>%
    map(., ~standard_Saline(.x)) %>%
    map(., ~standard_FacilityNumber(.x)) %>%
    map(., ~standard_SIC(.x)) %>%
    add_state()
  
  return(site_formatted)
  
}
