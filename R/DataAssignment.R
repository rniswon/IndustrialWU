
generate_blankcsv <- function(x) {
  blankdat <- data.frame(
    file = x,
    SiteDescriptions = NA,
    LocationInfo = NA,
    MonthlyData = NA,
    AnnualData = NA,
    Metadata = NA,
    Duplicate = NA,
    NotRelevant = NA
  )
  
  return(blankdat)
}



generate_blankHeaderCrosswalkcsv <- function(filledassignment) {
  
  filledfile <- filledassignment
  
  blanksiteDescripts <- filledfile %>% filter(SiteDescriptions == 1 | 
                                                LocationInfo == 1 |
                                                MonthlyData == 1 |
                                                AnnualData == 1 |
                                                Metadata == 1) %>%
    mutate(State = str_extract(file, "(?<=/)[[:alpha:]]{2}")) %>%
    select(State, file) %>%
    mutate(ValueType = NA, SourceType = NA, Category = NA, Saline = NA, 
           FacilityName = NA, FacilityName1 = NA, FacilityName2 = NA,
           FacilityNumber = NA, FacilityNumber1 = NA, FacilityNumber2 = NA,
           SourceName = NA, SourceName1 = NA, SourceName2 = NA,
           SourceNumber = NA, SourceNumber1 = NA, SourceNumber2 = NA, 
           NAICS = NA, SIC = NA, Description = NA, HUC8 = NA, HUC10 = NA, 
           HUC12 = NA, AquiferName1 = NA, AquiferName2 = NA,
           BasinName1 = NA, BasinName2 = NA, Address1 = NA, City1 = NA,
           County1 = NA, State1 = NA, Zip1 = NA, Address2 = NA, City2 = NA,
           County2 = NA, State2 = NA, Zip2 = NA, Lat = NA, Lon = NA, Datum = NA, 
           Projection = NA, Year = NA, Jan = NA, Feb = NA, Mar = NA, Apr = NA,
           May = NA, Jun = NA, Jul = NA, Aug = NA, Sep = NA, Oct = NA, Nov = NA,
           Dec = NA, Units_monthly = NA, Method_monthly = NA, Annual_reported = NA,
           Units_annual_reported = NA, Method_annual_reported = NA, DataProtected = NA)
  
  return(blanksiteDescripts)
}


get_filledcsv <- function(file) {
  read.csv(file, colClasses = "character")
}

merge_data <- function(blank, filled) {
  filledfile <- get_filledcsv(filled)
  fp_classified <- filledfile %>% na.omit() %>% dplyr::pull(file)
  
  fp_unclassified <- blank %>% filter(!file %in% fp_classified)
  
  d <- bind_rows(fp_unclassified, filledfile) %>% unique()
  write.csv(d, filled, row.names = FALSE)
  return(d)
}


readandrename_columns <- function(datafp, HeaderCrosswalk, pivots) {
  
  filledheader <- HeaderCrosswalk
  
  headers_classified <- filledheader %>% na.omit()
  
  dat <- imap(headers_classified$file, ~{
    
    i <- .y
    dat_raw <- read_in_datafile(datafp, .x)
    headercrosswalk <- headers_classified %>%
      slice(i) %>% select(where(~{. != ""})) %>% 
      {if(length(names(.)) > 2) {
        pivot_longer(., cols = -c(State, file), names_to = "NewName", 
                   values_to = "OldName")
      } else {.}}
    
    if("~PIVOT~" %in% headercrosswalk$OldName) {
      pivot_instr <- read.csv(pivots) %>% filter(file %in% headercrosswalk$file)
      
      if(nrow(pivot_instr) > 0) {
        
        
        mutatecode <- with(pivot_instr,
                           paste0(
                             ifelse(grepl("=", names_tofrom), paste0('mutate(., ', names_tofrom, ')'), "{.}"),
                             "%>%", ifelse(grepl("=", cols), paste0('mutate(., ', cols, ')'), "{.}")))
        
        selectcode <- 
          with(pivot_instr,
               {
               ifelse(long_wide == "wide",
                      {paste0('select(., any_of(c("', paste(
                                ifelse(
                                  any(grepl("=", c(names_tofrom, cols))), 
                                  paste(str_trim(unlist(str_extract_all(c(names_tofrom, cols), ".*(?==)"))), collapse = '", "'), 
                                  paste(c(names_tofrom, cols), sep = '", "')),
                                values_tofrom, 
                                paste(headercrosswalk$OldName, collapse = '", "'), sep = '", "'), '")))')}, "{.}")})
        
        pivotcode <- with(pivot_instr,
                          ifelse(long_wide == "long",
                                 paste0('pivot_longer(., ',
                                        paste0('cols = ', cols, ', '),
                                        'names_to = c(', paste0(
                                          '"', paste0(str_trim(unlist(str_split(names_tofrom, ","))), collapse = '", "'), '"'
                                        ), '), ',
                                        ifelse(grepl("sep:", names_pattern), 
                                               paste0('names_sep = ', gsub("sep:", "", names_pattern)), ''), ", ",
                                        'values_to = "', values_tofrom, '")'),
                                 paste0('pivot_wider(., ',
                                        'names_from = c("', ifelse(grepl("=", names_tofrom), str_trim(str_extract(names_tofrom, ".*(?==)")), names_tofrom), '"), ',
                                        'values_from = "', values_tofrom, '")')))
        
        filtercode <- with(pivot_instr, ifelse(long_wide == "long", paste0('filter(., !is.na(', values_tofrom, '))'), '{.}'))
        
        dat_rare <- dat_raw %>% 
          {eval(parse(text = mutatecode))} %>% 
          {eval(parse(text = selectcode))}  %>%
          {eval(parse(text = pivotcode))} %>% 
          {eval(parse(text = filtercode))}
        headercrosswalk <- headercrosswalk %>% mutate(OldName = case_when(OldName == "~PIVOT~" ~ NewName,
                                                                          TRUE ~ OldName))
      } else {
        stop(
          paste0(
            "Pivot instructions need to be entered into DataPivots.csv for file ", 
            unique(headercrosswalk$file)))}
      
    } else {dat_rare <- dat_raw}
    
    
    if(any(c("NewName", "OldName") %in% names(headercrosswalk))) {
      tmp <- map2_dfc(headercrosswalk$NewName, headercrosswalk$OldName, ~{
        x <- .x
        y <- unlist(str_split(.y, ", "))
        map_dfc(y, ~{
          tibble(!!x := dat_rare[[.x]])
        })
      })
    } else {tmp <- data.frame()}
tmp
  })
  names(dat) <- headers_classified$file
  
  return(dat)
}

reformat_data <- function(x, headers, hardcodedparams) {
    x_munged <- x %>% formatsitedata(., headers, hardcodedparams) %>% 
    formatlocationdata(., headers, hardcodedparams) %>%
    formatmonthlydata(., headers, hardcodedparams) %>% 
    formatannualdata(., headers, hardcodedparams) %>% 
    formatmetadata(., headers, hardcodedparams) %>%
    map(., ~unique(.x))
  
  x_munged_indices_bysize <- unlist(map(x_munged, ~length(.x))) %>% sort(decreasing = TRUE)
  
  x_merge_ready <- x_munged[names(x_munged_indices_bysize)] %>% keep(~{nrow(.) > 0})
  x_merged <- reduce(x_merge_ready, merge_andreplaceNA, .dir = "forward") 
  
  ordered <- names(headers)
  
  x_ordered <- x_merged %>% select(any_of(ordered))
  
  return(x_ordered)
}

merge_andreplaceNA <- function(x, y) {
  
  x_complete <- x %>% select(where(~!any(is.na(.))))
  y_complete <- y %>% select(where(~!any(is.na(.))))
  
  merge_vars <- names(x_complete)[names(x_complete) %in% names(y_complete)]
  
  merge <- rquery::natural_join(x, y, by = merge_vars, jointype = "FULL")
  
  return(merge)
}

add_state <- function(renamed_rawdat) {
  imap(renamed_rawdat, ~{
    state <- str_extract(.y, "(?<=^/)[[:alpha:]]{2}")
    .x %>% mutate(State = state)
  })
}

concat_columns <- function(data, Column) {
  tmp <- data %>%
    rowwise() %>%
    select(contains(Column)) %>% mutate(tmp = paste(na.omit(c_across(contains(Column))), collapse = ", ")) %>%
    pull(tmp)
  tmp2 <- data %>%
    select(-contains(Column)) %>% mutate(!!Column := tmp)
  return(tmp2)
}

handle_readmes <- function(data, fp, header, hardcodes) {
  info <- unlist(data[[header]])
  tmp <- handle_oddformats(data, fp, header, hardcodes, info)
}

handle_headers <- function(data, fp, header, headercrosswalk, hardcodedparams) {
  oldheader <- headercrosswalk %>% filter(file == fp) %>% pull(header)
  tmp <- handle_oddformats(data, fp, header, hardcodedparams, oldheader)
}

handle_oddformats <- function(data, fp, header, hardcodes, info) {
  datatype <- ifelse(grepl("Units", header), "Units", 
                     ifelse(grepl("Method", header), "Methods", 
                            ifelse(grepl("DataProtected", header), "Protection", "TBD")))
  
  if(datatype == "Units") {
    mgd_grex <- paste(mgd_options(), collapse = "|")
    found_units <- unique(gsub(mgd_grex, "mgd", na.omit(unlist(str_extract_all(info, mgd_grex)))))
    
    if(length(found_units) == 1) {
      tmp <- data %>% mutate(!!header := found_units)
    } else {tmp <- manual_update(data, fp, header, hardcodes)}
    
  } else if(datatype == "Methods") {
    
    permit_options <- "application|permitted|permited|authorized"
    report_options <- "reported|submitted"
    alloptions <- paste(permit_options, report_options, sep = "|")
    found_methods <- unique(
      gsub(report_options, "reported",
           gsub(permit_options, "permitted",
                na.omit(unlist(str_extract_all(info, alloptions))))))
    if(length(found_methods) == 1) {
      tmp <- data %>% mutate(!!header := found_methods)
    } else {tmp <- manual_update(data, fp, header, hardcodes)}
  } else if(datatype == "Protection") {
    true_options <- "No further distribution|expressed written approval"
    found_protectionstatus <- unique(gsub(true_options, "TRUE", na.omit(unlist(str_extract_all(info, true_options)))))
    if(length(found_protectionstatus) == 1) {
      tmp <- data %>% mutate(!!header := found_protectionstatus)
    } else {tmp <- manual_update(data, fp, header, hardcodes)}
  } else {}
  return(tmp)
}

manual_update <- function(data, fp, header, hardcodedparams) {
  hardparams <- read.csv(hardcodedparams, colClasses = "character")
  
  if(header %in% hardparams$Header) {
    found_param_manual <- hardparams %>% filter(Header == header) %>% pull(Value)
  } else {
    found_param_manual <- svDialogs::dlg_input(message = paste("Enter suspected", header, "value based on", fp, ". Suggested options are", paste(found_methods, collapse = ", ")))$res
    hardparams_update <- hardcodedparams %>% add_row(file = fp, Header = header, Value = found_param_manual)
    
    write.csv(hardparams_update, file = hardcodedparams, row.names = FALSE)
  }
  tmp <- data %>% mutate(!!header := found_param_manual)
  
  return(tmp)
}

mgd_options <- function() {c("mgd", "MGD", "Mgald", "Mgd", "million gallons per day", "Mgal/d")}
Estimated_methods <- function() {c("CTDEEP_2021_Est", "CTDEEP_Estimated", "Estimated")}
Reported_methods <- function() {c("CTDEEP_Reported", "PA 02-102", "Reported")}
Unknown_methods <- function() {c("Unknown")}
