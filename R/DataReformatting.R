detect_readme <- function(filename) {
  grepl("ReadMe|readme|Read_Me|read_me|metadata", filename)
}

convert2decimal <- function(x) {
  x <- gsub("^0", "", as.character(x))
  degrees <- suppressWarnings(as.numeric(stringr::str_sub(x, 1, 2)))
  minutes <- suppressWarnings(as.numeric(stringr::str_sub(x, 3, 4)))
  seconds <- suppressWarnings(as.numeric(stringr::str_sub(x, 5, nchar(x))))
  
  round(degrees + (minutes + (seconds / 60)) / 60, 6)
}

handle_coordinates <- function(data, header) {
  tmp <- data |>
    dplyr::mutate(dplyr::across(contains(header), ~{
      dplyr::case_when(
        . == "0" ~ NA_character_,
        . == "NULL" ~ NA_character_,
        stringr::str_detect(., "^[[:digit:]]{2}\\.") ~ as.character(.),
        stringr::str_detect(., "^[[:digit:]]{6}\\.?") ~ as.character(convert2decimal(.)),
        .default = as.character(.)
      )
    })) 
  
  tmp
}


merge_andreplaceNA <- function(x, y) {
  
  x_complete <- x |> dplyr::select(where(~!any(is.na(.))))
  y_complete <- y |> dplyr::select(where(~!any(is.na(.))))
  
  merge_vars <- names(x_complete)[names(x_complete) %in% names(y_complete)]
  
  if(
    max(dplyr::pull(
      dplyr::summarize(dplyr::group_by(y, dplyr::across(all_of(merge_vars))), n = dplyr::n(), .groups = "drop"), 
      n)) > length(merge_vars)) {
    datavars <- c("Annual_reported", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Sep", "Oct", "Nov", "Dec", "Year", "Category")
    
    y_unique <- y |> 
      dplyr::group_by(dplyr::across(any_of(c(merge_vars, datavars)))) |> 
      dplyr::summarize(dplyr::across(.cols = everything(), .fns = ~paste(unique(.), collapse = ", ")), .groups = "drop") |>
      dplyr::mutate(dplyr::across(any_of(c("Lat", "Lon", "SourceNumber")), ~gsub(",.*", "", .))) ## If more than one location or source, keep only the first one. Source will only treated this way if it isn't used to merge - so if the other data is only to the facility level, not the source level.
  } else {y_unique <- y}
  
  merge <- rquery::natural_join(x, y_unique, by = merge_vars, jointype = "FULL")
  
  return(merge)
}


add_state <- function(renamed_rawdat) {
  purrr::imap(renamed_rawdat, ~{
    state <- stringr::str_extract(.y, "(?<=^/)[[:alpha:]]{2}")
    .x |> dplyr::mutate(State = state)
  })
}

concat_columns <- function(data, Column) {
  tmp <- data |>
    dplyr::rowwise() |>
    dplyr::select(contains(Column)) |> 
    dplyr::mutate(tmp = paste(unique(na.omit(gsub("NULL", NA_character_, 
                                           dplyr::c_across(contains(Column))))), 
                       collapse = ", ")) |>
    dplyr::pull(tmp)
  tmp2 <- data |>
    dplyr::select(-contains(Column)) |> dplyr::mutate(!!Column := tmp)
  return(tmp2)
}

handle_readmes <- function(data, fp, header, updatedCrosswalks, existingCrosswalks) {
  info <- unlist(data[[header]])
  tmp <- handle_oddformats(data, fp, header, updatedCrosswalks, existingCrosswalks, info)
}

handle_headers <- function(data, fp, header, updatedCrosswalks, existingCrosswalks) {
  oldheader <- updatedCrosswalks$HeaderCrosswalk |> dplyr::filter(file == fp) |> dplyr::pull(header)
  tmp <- handle_oddformats(data, fp, header, updatedCrosswalks, existingCrosswalks, info = oldheader)
}

handle_oddformats <- function(data, fp, header, updatedCrosswalks, existingCrosswalks, info) {
  datatype <- ifelse(grepl("Units", header), "Units", 
                     ifelse(grepl("Method", header), "Method", 
                            ifelse(grepl("DataProtected", header), "Protect", 
                                   ifelse(grepl("ValueType", header),
                                          "ValueType", 
                                          ifelse(grepl("SourceType", header), 
                                                 "SourceType", 
                                                 ifelse(grepl("Category", 
                                                              header), 
                                                        "Category", "TBD"))))))
  if(datatype == "TBD") {stop("New use of ReadMe files detected")}
  
  crosswalk_tmp <- updatedCrosswalks$DataCodesCrosswalk |>
    dplyr::filter(grepl(datatype, header))
  found_options <- na.omit(crosswalk_tmp$new_value[unique(unlist(purrr::map(crosswalk_tmp$original_value, ~grep(.x, info))))])
  if(length(found_options) == 1) {
    tmp <- data |> dplyr::mutate(!!header := found_options)
  } else {tmp <- manual_update(data, fp, header, updatedCrosswalks, existingCrosswalks, found_options)}
  
  return(tmp)
}

manual_update <- function(data, fp, header, updatedCrosswalks, existingCrosswalks, inputoptions) {
  
  hardparams <- updatedCrosswalks$HardcodedManualAttributes
  if(header %in% (hardparams$Header[hardparams$file == fp])) {
    found_param_manual <- hardparams |> dplyr::filter(Header == header, file == fp) |> dplyr::pull(Value)
  } else {
    found_param_manual <- svDialogs::dlg_input(message = paste("Enter suspected", header, "value based on", fp, ". Suggested options are", paste(inputoptions, collapse = ", ")))$res
    hardparams_update <- hardparams |> add_row(file = fp, Header = header, Value = found_param_manual)
    
    write.csv(hardparams_update, file = file.path(existingCrosswalks, "HardcodedManualAttributes.csv"), row.names = FALSE)
  }
  tmp <- data |> dplyr::mutate(!!header := found_param_manual)
  
  return(tmp)
}



crosswalk_codes <- function(data, fp, header, codescrosswalk, forceupdate = TRUE) {
  header_tmp <- header
  codecrosswalk <- codescrosswalk |> 
    dplyr::filter(header == header_tmp)
  if(forceupdate) {
    if(any(!unique(data[[header]]) %in% codecrosswalk$original_value)) {
      stop(paste("New codes", 
                 paste(unique(data[[header]])[!unique(data[[header]]) %in% codecrosswalk$original_value], collapse = ", "), 
                 "detected for", header, "in", fp))
    }
  }
  
  crosswalk <- codecrosswalk |> dplyr::select(original_value, new_value) |>
    dplyr::mutate(new_value = dplyr::case_when(grepl("NA", new_value) ~ new_value,
                                 .default = paste0('"', new_value, '"'))) |>
    dplyr::summarize(
      original_value = paste0('c("', paste(original_value, collapse = '", "'), '")'),
      .by = new_value) |>
    dplyr::mutate(expr = paste0(original_value, " ~ ", new_value))
  
  mutatecode <- ifelse(length(crosswalk$expr) > 0,
                       paste0(
                         "dplyr::mutate(., ", header_tmp, " = ", "dplyr::case_match(", header_tmp, ", ", paste(crosswalk$expr, collapse = ", "), ", .default = ", header_tmp, "))"
                       ),
                       ".")
  
  tmp <- data %>% {eval(parse(text = mutatecode))}
  
}


standard_datacodestreatment <- function(data, filename, header, updatedCrosswalks, existingCrosswalks, force = c(TRUE, FALSE)) {
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      if(detect_readme(filename)) {
        tmp <- handle_readmes(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else if(!is.character(data[[header]])) {
        tmp <- handle_headers(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else if(all(is.na(data[[header]]))) {
        tmp <- handle_headers(data, filename, header, updatedCrosswalks, existingCrosswalks)
        } else if(is.character(data[[header]])) {
        tmp <- crosswalk_codes(data = data, fp = filename, header = header, codescrosswalk = updatedCrosswalks$DataCodesCrosswalk, forceupdate = force)}
    } else if(length(grep(header, names(data))) > 1) {
      tmp <- concat_columns(data, header) |> 
        crosswalk_codes(fp = filename, header = header, codescrosswalk = updatedCrosswalks$DataCodesCrosswalk, forceupdate = force)
    }
    
  } else (tmp <- data)
  tmp
}

standard_nametreatment <- function(data, header) {
  if(length(grep(header, names(data))) > 0) {
    if(sum(names(data) == header) == 1) {
      tmp <- data %>%
        dplyr::mutate(!!header := fedmatch::clean_strings(as.character(.[[header]]), common_words = fedmatch::corporate_words))
    } else if(length(grep(header, names(data))) > 1) {
      tmp <- concat_columns(data, header) %>% 
        dplyr::mutate(!!header := fedmatch::clean_strings(as.character(.[[header]]), common_words = fedmatch::corporate_words))
    }
    
  } else (tmp <- data)
  tmp
}

standard_idtreatment <- function(data, header) {
  if(sum(names(data) == header) == 1) {
    tmp <- data %>%
      dplyr::mutate(!!header := as.character(.[[header]]))
  } else if(length(grep(paste0(header, ".."), names(data))) > 1) {
    tmp <- concat_columns(data, header) %>% 
      dplyr::mutate(!!header := as.character(.[[header]]))
  } else (tmp <- data)
  tmp
}

standard_HUCtreatment <- function(data, header) {
  if(header %in% names(data)) {
    n <- readr::parse_number(header)
    tmp <- data |> dplyr::mutate(!!header := substr(as.character(data[[header]]), 1, n))
  } else (tmp <- data)
}

standard_Addresstreatment <- function(data, header) {
  if(header %in% names(data)) {
    entrylines <- stringr::str_split(data[[header]], ",") |> purrr::map(~stringr::str_trim(.x))
    type <- stringr::str_extract(header, "Address|City|State|Zip")
    
    state_regex <- paste0(
      "(", paste(state.name, state.abb, stringr::str_to_upper(state.name), sep = "|", collapse = "|"), ")(?!-)")
    zip_regex <- "[[:digit:]]{5}(-[[:digit:]]{4})?( - [[:digit:]]{4})?$"
    
    tmpvals <- purrr::map_chr(entrylines, ~{
      
      x_clean <- subset(unique(unlist(stringr::str_split(.x, "\r|\n"))), 
                        unique(unlist(stringr::str_split(.x, "\r|\n"))) != "")
      index_zip <- stringr::str_which(x_clean, zip_regex)
      index_state <- stringr::str_which(x_clean, state_regex)
      
      if(length(index_state) > 1) {
        if(any(index_state %in% index_zip)) {
          if(sum(index_state %in% index_zip) == 1) {
            index_state <- subset(index_state, index_state %in% index_zip)
          } else {index_state <- integer(0)}
        } else if(any(x_clean %in% c(state.abb, state.name))) {
          index_state <- which(x_clean %in% c(state.abb, state.name))
        }
        if(length(index_zip) > 1) {
          if(any(index_zip %in% index_state)) {
            index_zip <- subset(index_zip, index_zip %in% index_state)
          } else {index_zip <- integer(0)}
        }
      }
      
      if(length(x_clean) == 0) {
        address <- city <- state <- zip <- NA_character_
      } else if(length(x_clean) == 1) {
        address <- x_clean
        city <- x_clean
        state <- stringr::str_extract(x_clean, state_regex)
        zip <- stringr::str_extract(x_clean, zip_regex)
      } else if(length(x_clean) > 1) {
        if(length(c(index_zip, index_state)) > 0) {
          address <- x_clean[-c(index_state, index_zip)][grep("[[:digit:]]", x_clean[-c(index_state, index_zip)])]
          city <- x_clean[-c(index_state, index_zip)][which(!grepl("[[:digit:]]", x_clean[-c(index_state, index_zip)]))]
          state <- stringr::str_extract(x_clean[index_state], state_regex)
          zip <- stringr::str_extract(x_clean[index_zip], zip_regex)
        } else {
          address <- paste(x_clean, collapse = ", ")
          city <- paste(x_clean, collapse = ", ")
          state <- stringr::str_extract(paste(x_clean, collapse = ", "), state_regex)
          zip <- stringr::str_extract(paste(x_clean, collapse = ", "), zip_regex)
        }}
      
      if(type == "Address") {tmp <- address} else if(
        type == "City") {tmp <- city} else if(
          type == "State") {tmp <- state} else if(
            type == "Zip") {tmp <- zip}
      if(length(tmp) == 0) {tmp <- NA_character_}
      if(length(tmp) > 1) {tmp <- paste(tmp, collapse = ", ")}
      tmp
    }
    )
    
    tmp <- data |>
      dplyr::mutate(!!header := tmpvals)
    
  } else {tmp <- data}
  tmp
}

standard_coordinatetreatment <- function(data, header) {
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      tmp <- handle_coordinates(data, header)
    } else if(length(grep(header, names(data))) > 1) {
      tmp1 <- handle_coordinates(data, header) |>
        concat_columns(header)  
      tmp <- tmp1 |> dplyr::mutate(!!header := gsub(",.*", "", tmp1[[header]]))
      
    }
  } else (tmp <- data)
  tmp
}

standard_Yeartreatment <- function(data, header) {
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      tmp <- data |> 
        dplyr::mutate(!!header := as.numeric(readr::parse_number(as.character(data[[header]])))) |>
        dplyr::filter(if_any(.cols = any_of(header), ~!is.na(.)))
    } else if(length(grep(header, names(data))) > 1) {
      stop("Is there more than one Year column?")
    }
  } else (tmp <- data)
  tmp
}

data_NAcodes <- c("", "n/a", "N/A", "NA", "NAN", "na", "nan", 
                  "not reported yet", "closed")

standard_datatreatment <- function(data, header) {
  if(length(grep(header, names(data))) > 0) {
    if(!is.numeric(data[[header]])) {
      if(!all(unique(gsub("[[:digit:]]*", "", data[[header]])) %in% data_NAcodes)) {
        stop("New non-numeric data value detected")
      } else {
        suppressWarnings({
          tmp <- data |>
            dplyr::mutate(!!header := as.numeric(data[[header]]))
        })
      }
    } else (tmp <- data)
  } else (tmp <- data)
  tmp
}

reformat_data <- function(x, updatedCrosswalks, existingCrosswalks) {
  
  list(standard_datacodestreatment, standard_nametreatment, standard_idtreatment, 
       standard_HUCtreatment, standard_Addresstreatment, standard_coordinatetreatment,
       standard_Yeartreatment) # call these to let the targets package know that they are used in this function
  
  datacodecolumns_force <- c("ValueType", "SourceType", "Category", "Saline", 
                             "Units_monthly", "Method_monthly", "Units_annual_reported",
                             "Method_annual_reported"
                             )
  datacodecolumns_unforce <- c("Description", "Datum", "Projection", "DataProtected")
  namecolumns <- c("FacilityName", "FacilityName1", "FacilityName2", "SourceName",
                   "SourceName1", "SourceName2", "AquiferName1", "AquiferName2",
                   "BasinName1", "BasinName2")
  idcolumns <- c("FacilityNumber", "FacilityNumber1", "FacilityNumber2", "SourceNumber",
                 "SourceNumber1", "SourceNumber2", "NAICS", "SIC", "County1",
                 "County2")
  HUCcolumns <- c("HUC8", "HUC10", "HUC12")
  Addresscolumns <- c("Address1", "City1", "State1", "Zip1", "Address2", "City2",
                      "State2", "Zip2")
  coordinatecolumns <- c("Lat", "Lon")
  Yearcolumns <- "Year"
  datacolumns <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                   "Sep", "Oct", "Nov", "Dec", "Annual_reported")
  
  datacodes_f_code <- paste0(
    "purrr::imap(., ~standard_datacodestreatment(.x, .y, '", datacodecolumns_force, 
    "', updatedCrosswalks, existingCrosswalks, force = TRUE))", collapse = " %>% "
  )
  datacodes_u_code <- paste0(
    "purrr::imap(., ~standard_datacodestreatment(.x, .y, '", datacodecolumns_unforce, 
    "', updatedCrosswalks, existingCrosswalks, force = FALSE))", collapse = " %>% "
  )
  names_code <- paste0("purrr::map(., ~standard_nametreatment(.x, '", namecolumns, "'))", collapse = " %>% ")
  ids_code <- paste0("purrr::map(., ~standard_idtreatment(.x, '", idcolumns, "'))", collapse = " %>% ")
  HUCs_code <- paste0("purrr::map(., ~standard_HUCtreatment(.x, '", HUCcolumns, "'))", collapse = " %>% ")
  Addresses_code <- paste0("purrr::map(., ~standard_Addresstreatment(.x, '", Addresscolumns, "'))", collapse = " %>% ")
  coordinates_code <- paste0("purrr::map(., ~standard_coordinatetreatment(.x, '", coordinatecolumns, "'))", collapse = " %>% ")
  years_code <- paste0("purrr::map(., ~standard_Yeartreatment(.x, '", Yearcolumns, "'))", collapse = " %>% ")
  data_code <- paste0("purrr::map(., ~standard_datatreatment(.x, '", datacolumns, "'))", collapse = " %>% ")
 
  x_munged <- x %>%
    {eval(parse(text = datacodes_u_code))} %>% # ~0 seconds
    {eval(parse(text = HUCs_code))} %>% # ~0 seconds
    {eval(parse(text = years_code))} %>% # ~ 1 second
    {eval(parse(text = ids_code))} %>% # ~3 seconds
    {eval(parse(text = datacodes_f_code))} %>% # ~12 seconds
    {eval(parse(text = names_code))} %>% # ~17 seconds
    {eval(parse(text = coordinates_code))} %>% # ~ 22 seconds
    {eval(parse(text = Addresses_code))} %>% # ~60 seconds
    {eval(parse(text = data_code))} |>
    purrr::map(~janitor::remove_empty(.x, which = c("rows", "cols"))) |>
    add_state() |>
    purrr::map(~unique(.x))
  
  if(any(grepl("\\.\\.\\.", unlist(purrr::map(x_munged, ~names(.x)))))) {
    issue <- unique(na.omit(
      stringr::str_extract(unlist(purrr::map(x_munged, ~names(.x))), ".*(?=\\.\\.\\.)")))
    stop(paste0("New case(s) for ", paste(issue, collapse = ", ")))
  }
  x_munged_indices_bysize <- unlist(purrr::map(x_munged, ~length(.x))) |> sort(decreasing = TRUE)
  x_merge_ready <- x_munged[names(x_munged_indices_bysize)] |> purrr::keep(~{nrow(.) > 0})
  x_bystate <- purrr::map(state.abb, ~{st <- .x; purrr::keep_at(x_merge_ready, ~grepl(paste0("/", st, "/"), .))}); names(x_bystate) <- state.abb
  x_readystates <- purrr::keep(x_bystate, ~length(.) > 0)
  x_simplestates <- purrr::map(x_readystates, ~{purrr::reduce(.x, merge_andreplaceNA, .dir = "forward")})
  
  x_all <- do.call("bind_rows", x_simplestates)
  
  ordered <- names(updatedCrosswalks$HeaderCrosswalk)
  
  x_ordered <- x_all |> dplyr::select(any_of(ordered))

  return(x_ordered)
}

write_allstates <- function(x) {
  write.csv(x, "FormattedDataOutputs/AllStates.csv", row.names = FALSE)
  save(x, file = "FormattedDataOutputs/AllStates.RDa")
  return("FormattedDataOutputs/AllStates.csv")
}
