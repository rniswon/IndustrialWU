
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


readandrename_columns <- function(datafp, HeaderCrosswalk, pivots, HardCodes) {
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
        new <- .x
        old <- unlist(str_split(.y, ", "))
        map_dfc(old, ~{
          old_sub <- .x
          if(old_sub %in% names(dat_rare)) {
            tmp <- tibble(!!new := dat_rare[[old_sub]])} else {
              if(old_sub %in% names(dat_raw)) {stop("Something went wrong in the pivots")} else 
                if(!exists("mutatecode")) {stop("Check entries in HeaderCrosswalk.csv that they match the data exactly.")} else {
                names_check <- dat_raw %>% 
                  {eval(parse(text = mutatecode))} %>% 
                  {eval(parse(text = selectcode))} %>%
                  {eval(parse(text = paste0("select(., ", str_extract(pivotcode, "(?<=cols = ).*(?=, names_to)"), ")")))} %>%
                  names()
                nm <- manual_update(data.frame(tmp = NA), unique(headercrosswalk$file), old_sub, HardCodes, names_check)
                
                tmp <- tibble(!!new := nm[[old_sub]])
                
              }
            }
          tmp
          
        })
      })
    } else {tmp <- data.frame()}
tmp
  })
  names(dat) <- headers_classified$file
  
  return(dat)
}

reformat_data <- function(x, headers, hardcodedparams, codescrosswalk) {

    x_munged <- x %>% formatsitedata(., hardcodedparams, codescrosswalk) %>% 
    formatlocationdata(., headers, hardcodedparams, codescrosswalk) %>%
    formatmonthlydata(., headers, hardcodedparams, codescrosswalk) %>% 
    formatannualdata(., headers, hardcodedparams, codescrosswalk) %>% 
    formatmetadata(., headers, hardcodedparams, codescrosswalk) %>%
    map(., ~unique(.x))
    
    if(any(grepl("\\.\\.\\.", unlist(map(x_munged, ~names(.x)))))) {
      issue <- unique(na.omit(
        str_extract(unlist(map(x_munged, ~names(.x))), ".*(?=\\.\\.\\.)")))
      stop(paste0("New case(s) for ", paste(issue, collapse = ", ")))
    }
  
  x_munged_indices_bysize <- unlist(map(x_munged, ~length(.x))) %>% sort(decreasing = TRUE)
  x_merge_ready <- x_munged[names(x_munged_indices_bysize)] %>% keep(~{nrow(.) > 0})
  x_bystate <- map(state.abb, ~{st <- .x; keep_at(x_merge_ready, ~grepl(paste0("/", st, "/"), .))}); names(x_bystate) <- state.abb
  x_readystates <- keep(x_bystate, ~length(.) > 0)
  x_simplestates <- map(x_readystates, ~reduce(.x, merge_andreplaceNA, .dir = "forward"))
  
  x_all <- do.call("bind_rows", x_simplestates)
  
  ordered <- names(headers)
  
  x_ordered <- x_all %>% select(any_of(ordered))
  
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
    select(contains(Column)) %>% 
    mutate(tmp = paste(na.omit(gsub("NULL", NA_character_, 
                                    c_across(contains(Column)))), 
                       collapse = ", ")) %>%
    pull(tmp)
  tmp2 <- data %>%
    select(-contains(Column)) %>% mutate(!!Column := tmp)
  return(tmp2)
}

handle_readmes <- function(data, fp, header, hardcodes, codescrosswalk) {
  info <- unlist(data[[header]])
  tmp <- handle_oddformats(data, fp, header, hardcodes, info, codescrosswalk)
}

handle_headers <- function(data, fp, header, headercrosswalk, hardcodedparams, codescrosswalk) {
  oldheader <- headercrosswalk %>% filter(file == fp) %>% pull(header)
  tmp <- handle_oddformats(data, fp, header, hardcodedparams, oldheader, codescrosswalk)
}

handle_oddformats <- function(data, fp, header, hardcodes, info, codescrosswalk) {
  datatype <- ifelse(grepl("Units", header), "Units", 
                     ifelse(grepl("Method", header), "Method", 
                            ifelse(grepl("DataProtected", header), "Protect", 
                                   ifelse(grepl("ValueType", header), "ValueType", "TBD"))))
  if(datatype == "TBD") {stop("New use of ReadMe files detected")}
  
  crosswalk_tmp <- read.csv(codescrosswalk, colClasses = "character") %>%
    filter(grepl(datatype, header))
  found_options <- na.omit(crosswalk_tmp$new_value[unique(unlist(map(crosswalk_tmp$original_value, ~grep(.x, info))))])
  if(length(found_options) == 1) {
    tmp <- data %>% mutate(!!header := found_options)
  } else {tmp <- manual_update(data, fp, header, hardcodes, found_options)}
 
  return(tmp)
}

manual_update <- function(data, fp, header, hardcodedparams, inputoptions) {
  
  hardparams <- read.csv(hardcodedparams, colClasses = "character")
  if(header %in% (hardparams$Header[hardparams$file == fp])) {
    found_param_manual <- hardparams %>% filter(Header == header, file == fp) %>% pull(Value)
  } else {
    found_param_manual <- svDialogs::dlg_input(message = paste("Enter suspected", header, "value based on", fp, ". Suggested options are", paste(inputoptions, collapse = ", ")))$res
    hardparams_update <- hardparams %>% add_row(file = fp, Header = header, Value = found_param_manual)
    
    write.csv(hardparams_update, file = hardcodedparams, row.names = FALSE)
  }
  tmp <- data %>% mutate(!!header := found_param_manual)
  
  return(tmp)
}



crosswalk_codes <- function(data, fp, header, codescrosswalk, forceupdate = TRUE) {
  header_tmp <- header
  codecrosswalk <- read.csv(codescrosswalk, colClasses = "character") %>% 
    filter(header == header_tmp)
  if(forceupdate) {
    if(any(!unique(data[[header]]) %in% codecrosswalk$original_value)) {
      stop(paste("New codes", 
                 paste(unique(data[[header]])[!unique(data[[header]]) %in% codecrosswalk$original_value], collapse = ", "), 
                 "detected for", header, "in", fp))
    }
  }
  
  crosswalk <- codecrosswalk %>% select(original_value, new_value) %>%
    mutate(new_value = case_when(grepl("NA", new_value) ~ new_value,
                                 .default = paste0('"', new_value, '"'))) %>%
    summarize(
      original_value = paste0('c("', paste(original_value, collapse = '", "'), '")'),
      .by = new_value) %>%
    mutate(expr = paste0(original_value, " ~ ", new_value))
  
  mutatecode <- paste0(
    "mutate(., ", header_tmp, " = ", "case_match(", header_tmp, ", ", paste(crosswalk$expr, collapse = ", "), ", .default = ", header_tmp, "))"
    )
  
  tmp <- data %>% {eval(parse(text = mutatecode))}
      
}