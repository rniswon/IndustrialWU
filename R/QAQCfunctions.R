parse_results <- function(alldata = data.frame(), groupings = c()) {
  tmp <- alldata |> 
    tidytable::group_split(any_of(groupings), .named = TRUE) |> 
    purrr::map(~as.data.frame(.x))
  return(tmp)
}

search_missingdata <- function(split_data) {
  flaggedmaybemissing <- purrr::map_lgl(split_data, ~{
    dplyr::select(.x, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Annual_reported) |> 
      janitor::remove_empty(which = "cols") |>
      rowSums(na.rm = FALSE) |> is.na() |> any()
    })
  return(split_data[flaggedmaybemissing])
}

search_duplicatedata <- function(split_data) {
  flaggedmaybeduplicate <- purrr::map_lgl(split_data, ~{
    fulldat <- dplyr::select(.x, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Annual_reported) |> 
      janitor::remove_empty(which = "rows") 
    
    uniquedat <- fulldat |> unique()
    
    nrow(fulldat) != nrow(uniquedat)
  })
  return(split_data[flaggedmaybeduplicate])
}

describe_missingproblems <- function(df, cols) {
  df |> janitor::remove_empty(which = "cols") |> 
    dplyr::filter(if_all(.cols = any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                   "Annual_reported")),
                      .fns = ~is.na(.))) |> dplyr::select(any_of(cols))
}

describe_duplicateproblems <- function(df, cols) {
  df |> janitor::remove_empty(which = "cols") |>
    dplyr::group_by(pick(any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                             "Annual_reported")))) |>
    dplyr::filter(dplyr::n() > 1) |> dplyr::ungroup() |>
    dplyr::filter(if_any(.cols = any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                   "Annual_reported")),
                  .fns = ~(as.numeric(.) != 0))) |> dplyr::select(any_of(cols))
}

checkQAQCstatus <- function(newdf, previousQAQCstatus) {
  dat_toQAQC <- parse_results(newdf, c("State"))
  dat_maybemissing <- search_missingdata(dat_toQAQC)
  missingissues <- data.frame(
    dat = names(dat_maybemissing), 
    n_maybenotmerged = map_dbl(dat_maybemissing, 
                               ~nrow(
                                 describe_missingproblems(.x, 
                                                          c("ValueType", "FacilityName", 
                                                            "FacilityNumber", "SourceName", "Year")))))
  dat_maybeduplicated <- search_duplicatedata(dat_toQAQC)
  duplicateissues <- data.frame(
    dat = names(dat_maybeduplicated), 
    n_maybeduplicated = map_dbl(dat_maybeduplicated, 
                                ~nrow(
                                  describe_duplicateproblems(.x, 
                                                             c("ValueType", "FacilityName", 
                                                               "FacilityNumber", "SourceName", "Year")))))
  biggestissues <- full_join(missingissues, duplicateissues, by = "dat") |>
    dplyr::filter(n_maybenotmerged > 0 | n_maybeduplicated  > 0) %>%
    arrange(dat)|> dplyr::rename(State = dat) |>
    dplyr::mutate(dplyr::across(.cols = everything(), .fns = ~as.character(.)))
  
  previousQAQCstatusread <- get_filledcsv(previousQAQCstatus)
  
  newQAQC <- map_dfr(biggestissues$State, ~{
    new <- dplyr::filter(biggestissues, State == .x)
    old <- dplyr::filter(previousQAQCstatusread, State == .x)
    if(any(new$n_maybeduplicated != old$n_maybeduplicated,
       new$n_maybenotmerged != old$n_maybenotmerged, na.rm = TRUE)) {
      tmp <- new %>% dplyr::mutate(lastupdate = as.character(Sys.Date()), Checked = "No") 
    } else {tmp <- old}
    tmp
  })
  
  write.csv(newQAQC, previousQAQCstatus, row.names = FALSE)
  
  unQAQCdstates <- newQAQC %>% dplyr::filter(Checked == "No") %>% pull(State) %>% sort()
  
  if(length(unQAQCdstates) > 0) {
    message(paste("QAQC recommended for the following state(s):", paste(unQAQCdstates, collapse = ", ")))
  }
  
  return(newQAQC)
}

# Also need to test locations of points

generate_statusupdate <- function(siteselectionmerged,
                                  FormattedSiteSelectiondata, 
                                  updatedCrosswalks,
                                  SWUDS,
                                  SiteSelection,
                                  status,
                                  QAQCupdate) {
  data_points <- siteselectionmerged %>%
    dplyr::mutate(SWUDS = str_detect(DataSource, basename(SWUDS)),
           SS_merged = str_detect(DataSource, basename(SiteSelection))) %>% 
    dplyr::group_by(State) %>% 
    dplyr::summarize(`Number of Data Points` = dplyr::n(), 
              `Number of SWUDS data points` = sum(SWUDS),
              `Number of NonSWUDS data points` = sum(!SWUDS),
              `Number of Data Points linked to Site Selection Sites` = sum(SS_merged))
  
  merged_sites <- siteselectionmerged %>% 
    dplyr::pull(SITESELECTION_FACILITYID) %>%
    str_split(" _OR_ ") %>%
    unique() %>%
    unlist() %>%
    unique()
  
  unmerged_sites <- FormattedSiteSelectiondata %>%
    dplyr::filter(!SITESELECTION_FACILITYID %in% merged_sites) %>%
    dplyr::group_by(State) %>%
    dplyr::summarize(`Number of Site Selection Sites NOT Represented` = dplyr::n())
  
  unprocessedstates <- data.frame(fp = unique(c(
    flag_unprocessedfiles(updatedCrosswalks$DataDirectories),
    flag_unprocessedfiles(updatedCrosswalks$HeaderCrosswalk))) %>% sort()) %>%
    dplyr::mutate(State = str_extract(fp, "(?<=/)[[:upper:]]{2}(?=/)"),
           access = str_detect(fp, ".accdb")) %>%
    dplyr::group_by(State) %>%
    dplyr::summarize(`Number of NonSWUDS files left to process` = dplyr::n(), 
                     AccessDB = any(access))
  
  QAQC_tomerge <- QAQCupdate %>%
    select(
      State, 
      `Number of sites perhaps improperly not merged` = n_maybenotmerged,
      `Number of data points potentially duplicated` = n_maybeduplicated,
      `QAQC check performed` = Checked
    ) %>%
    mutate(across(contains("Number"), ~as.integer(.)))
  
  newstatusupdate <- dplyr::full_join(
    dplyr::full_join(data_points, unmerged_sites, by = "State"),
    dplyr::full_join(unprocessedstates, QAQC_tomerge, by = "State"), by = "State") %>%
    dplyr::filter(!is.na(State)) %>%
    dplyr::arrange(State) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("Number"), ~replace_na(., 0)),
           AccessDB = case_when(AccessDB == FALSE ~ NA,
                                AccessDB == TRUE ~ TRUE),
           `NonSWUDS Data Processing Status` = dplyr::case_when(
             `Number of NonSWUDS files left to process` == 0 ~ dplyr::case_when(
               `Number of NonSWUDS data points` == 0 ~ "not needed",
               `Number of NonSWUDS data points` != 0 ~ "completed"
             )
           )
    ) %>%
    dplyr::mutate(`Date of Last Update` = Sys.Date())
  
  previousstatusread <- get_filledcsv(status) %>%
    as_tibble() %>%
    dplyr::rename_with(~str_replace_all(., "\\.", " ")) %>%
    dplyr::mutate(dplyr::across(contains("Number"), ~as.integer(.)))
    
  
  statuslinesupdate <-  map_dfr(newstatusupdate$State, ~{
    new <- dplyr::filter(newstatusupdate, State == .x)
    old <- dplyr::filter(previousstatusread, State == .x)
    n_columns <- str_subset(c(names(new), names(old)), "Number") %>% unique()
    if(any(map_lgl(n_columns, ~{
      new[[.x]] != old[[.x]]
    }), na.rm = TRUE)) {
      tmp <- new
    } else {tmp <- old}
    tmp
  })
  
  write.csv(statuslinesupdate, status, row.names = FALSE)
  
  previoustasks_people <- previousstatusread %>% pull(`Person Currently Working On`) %>% unique()
  remainingtasks_people <- statuslinesupdate %>% pull(`Person Currently Working On`) %>% unique()

  available_people <- str_subset(remainingtasks_people, previoustasks_people, negate = TRUE)
  if(length(available_people) > 0) {
    message(paste(paste(available_people, collapse = ", "), "possibly available for another state"))
  }

  return(statuslinesupdate)
  

  
}