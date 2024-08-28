parse_results <- function(alldata = data.frame(), groupings = c()) {
  tmp <- alldata |> 
    tidytable::group_split(any_of(groupings), .named = TRUE) |> 
    purrr::map(~as.data.frame(.x))
  return(tmp)
}

search_missingdata <- function(split_data) {
  flaggedmaybemissing <- purrr::map_lgl(split_data, ~{
    select(.x, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Annual_reported) |> 
      janitor::remove_empty(which = "cols") |>
      rowSums(na.rm = FALSE) |> is.na() |> any()
    })
  return(split_data[flaggedmaybemissing])
}

search_duplicatedata <- function(split_data) {
  flaggedmaybeduplicate <- purrr::map_lgl(split_data, ~{
    fulldat <- select(.x, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, Annual_reported) |> 
      janitor::remove_empty(which = "rows") 
    
    uniquedat <- fulldat |> unique()
    
    nrow(fulldat) != nrow(uniquedat)
  })
  return(split_data[flaggedmaybeduplicate])
}

describe_missingproblems <- function(df, cols) {
  df |> janitor::remove_empty(which = "cols") |> 
    filter(if_all(.cols = any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                   "Annual_reported")),
                      .fns = ~is.na(.))) |> select(any_of(cols))
}

describe_duplicateproblems <- function(df, cols) {
  df |> janitor::remove_empty(which = "cols") |>
    dplyr::group_by(pick(any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                             "Annual_reported")))) |>
    dplyr::filter(n() > 1) |> ungroup() |> 
    filter(if_any(.cols = any_of(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", 
                                   "Annual_reported")),
                  .fns = ~(as.numeric(.) != 0))) |> select(any_of(cols))
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
    filter(n_maybenotmerged > 0 | n_maybeduplicated  > 0) %>%
    arrange(dat)|> rename(State = dat) |>
    mutate(across(.cols = everything(), .fns = ~as.character(.)))
  
  previousQAQCstatusread <- get_filledcsv(previousQAQCstatus)
  
  newQAQC <- map_dfr(biggestissues$State, ~{
    new <- filter(biggestissues, State == .x)
    old <- filter(previousQAQCstatusread, State == .x)
    if(any(new$n_maybeduplicated != old$n_maybeduplicated,
       new$n_maybenotmerged != old$n_maybenotmerged, na.rm = TRUE)) {
      tmp <- new %>% mutate(lastupdate = as.character(Sys.Date()), Checked = "No") 
    } else {tmp <- old}
    tmp
  })
  
  write.csv(newQAQC, previousQAQCstatus, row.names = FALSE)
  
  unQAQCdstates <- newQAQC %>% filter(Checked == "No") %>% pull(State)
  
  if(length(unQAQCdstates) > 0) {
    message(paste("QAQC recommended for the following state(s):", paste(unQAQCdstates, collapse = ", ")))
  }
  
  return(newQAQC)
}

# Also test locations of points