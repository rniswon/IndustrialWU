standard_HUC8 <- function(data){
  if("HUC8" %in% names(data)) {
    tmp <- data %>%
      mutate(HUC8 = substr(as.character(HUC8), 1, 8))
  } else (tmp <- data)
  tmp
}

standard_HUC10 <- function(data){
  if("HUC10" %in% names(data)) {
    tmp <- data %>%
      mutate(HUC10 = substr(as.character(HUC10), 1, 10))
  } else (tmp <- data)
  tmp
}

standard_HUC12 <- function(data){
  if("HUC12" %in% names(data)) {
    tmp <- data %>%
      mutate(HUC12 = substr(as.character(HUC12), 1, 12))
  } else (tmp <- data)
  tmp
}



formatlocationdata <- function(renamed_rawdat, HeaderCrosswalk, hardcodedparams) {
  location_formatted <- renamed_rawdat %>%
    map(., ~standard_HUC8(.x)) %>%
    map(., ~standard_HUC10(.x)) %>%
    map(., ~standard_HUC12(.x)) %>%
    add_state()
  
  return(location_formatted)
}
