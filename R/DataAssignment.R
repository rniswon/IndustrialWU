
shapefileextensions <- c(".shp", ".dbf", ".htm", ".prj", ".sbn", ".sbx", 
                         ".shp.xml", ".shx")

read_in_datafile <- function(datafp, fp) {
  data <- if(grepl("\\~\\$", fp)) {
    list("Temporary and/or corrupted file")
  } else if(
    grepl(".csv|.txt|.rdb", fp)) {
    read.csv(file.path(datafp, fp), fill = TRUE, header = FALSE)
  } else if(grepl(".xlsx|.xls", fp)) {
    workbook_fp <- stringr::str_extract(fp, ".*(?=\\$)")
    sheetnm <- stringr::str_extract(fp, "(?<=\\$).*")
    suppressWarnings(suppressMessages(
      readxl::read_excel(file.path(datafp, workbook_fp), sheet = sheetnm)))
  } else if(grepl(".docx", fp)) {
    dat <- officer::read_docx(file.path(datafp, fp))
    txt <- officer::docx_summary(dat)$text
    data.frame(text = txt)
  } else if (grepl(paste(shapefileextensions, collapse = "|"), fp)) {
    fp_shp <- gsub(paste(shapefileextensions, collapse = "|"), ".shp", fp)
    dat <- sf::st_read(file.path(datafp, fp_shp), quiet = TRUE)
  } else if (grepl(".pdf", fp)) {
    dat <- 
      purrr::imap_dfr(stringr::str_split(pdftools::pdf_text(pdf = file.path(datafp, fp)), "\n"), 
               ~{data.frame(text = .x) |> dplyr::mutate(page = .y)})
    data
  } else {
    stop(paste0("New database type found that has not been built in yet (", fp, ")"))
  }
  data
}

split_forms <- function(data, form_df) {
  names(data) <- names(form_df)
  usefulrows <- which(!rowSums(is.na(form_df)) == length(form_df))
  rowsplits <- c(0, cumsum(diff(usefulrows) > 1))
  
  forms_vsplit <- form_df %>% dplyr::slice(usefulrows) %>%
    dplyr::mutate(group = rowsplits) %>%
    group_by(group) %>%
    dplyr::group_split(.keep = FALSE)
  
  data_vsplit <- data %>% dplyr::slice(usefulrows) %>%
    dplyr::mutate(group = rowsplits) %>%
    group_by(group) %>%
    dplyr::group_split(.keep = FALSE)
  
  
  hsplits <- purrr::map(forms_vsplit,
                        ~{subdf <- .x
                        usefulcols <- colSums(is.na(.x)) == nrow(.x)
                        colsplits <- c(0, cumsum(diff(usefulcols) > 1))
                        colsplits})
  
  forms_vhsplit <- map2(forms_vsplit, hsplits, ~{
    x <- .x; y <- .y
    purrr::map(unique(y), ~{x[,which(y == .x)]})
  }) %>% list_flatten()
  
  data_vhsplit <- map2(data_vsplit, hsplits, ~{
    x <- .x; y <- .y
    purrr::map(unique(y), ~{x[,which(y == .x)]})
  }) %>% list_flatten()
  
  usefulsplits <- map_lgl(forms_vhsplit, ~{any(c("~HEADER~", "~DATA~") %in% unique(unlist(.x)))})
  
  return(list_transpose(list(data = data_vhsplit[usefulsplits], form = forms_vhsplit[usefulsplits]), simplify = FALSE))
}

munge_forms <- function(dataformlist) {
  nativeformats <- map(dataformlist, ~{
    checkformtype(.x$form)
  })
  map2(dataformlist, nativeformats, ~{
    if(.y == "tidy") {
      .x$data[,which(.x$form[1,] == "~HEADER~")] %>% janitor::row_to_names(1)
    } else if(.y == "compoundheader") {
      headers <- .x$data[c(1, find_header(.x$data)),] %>% map(., ~paste(na.omit(unique(.x)), collapse = "_"))
      data <- .x$data[-c(1, find_header(.x$data)),]
      
      rbind(headers, data) %>% row_to_names(1)
      } else {browser()}
  })
}

checkformtype <- function(form) {
  
  type <- ifelse(janitor::find_header(form) == 1 & (!"~DATA~" %in% unlist(form[1,])) & 
                   (!"~HEADER~" %in% unlist(form[-1,])),
                 "tidy",
                 ifelse(
                   janitor::find_header(form) > 1 & 
                     (!"~DATA~" %in% unlist(form[c(1:janitor::find_header(form)),])) &
                     (!"~HEADER~" %in% unlist(form[-c(1:janitor::find_header(form)),])),
                   "compoundheader",
                   "TBD"
                 ))
  return(type)
}

readandrename_columns <- function(datafp, updatedCrosswalks, existingCrosswalks) {
  
  filledheader <- updatedCrosswalks$HeaderCrosswalk
  
  headers_classified <- filledheader |> na.omit() |> dplyr::filter(State %in% state.abb)
  
  dat <- purrr::imap(headers_classified$file, ~{
    i <- .y 
    
    dat_raw <- read_in_datafile(datafp, .x)
    headercrosswalk <- headers_classified |>
      dplyr::slice(i) |> dplyr::select(where(~{. != ""})) %>% 
      {if(length(names(.)) > 2) {
        tidyr::pivot_longer(., cols = -c(State, file), names_to = "NewName", 
                     values_to = "OldName")
      } else {.}}
    
    if(any(grepl("~FORM~", headercrosswalk$OldName))) {
      dat_raw <- rbind(names(dat_raw), dat_raw) %>%
        dplyr::mutate(across(everything(), ~gsub("\\.\\.\\.[[:digit:]]+", NA_character_, .))) %>%
        dplyr::mutate(across(everything(), ~gsub("^$", NA_character_, .)))
      if(any(grepl(unique(headercrosswalk$State), names(updatedCrosswalks$Forms)))) {
        pulledform <- updatedCrosswalks$Forms[[paste0(unique(headercrosswalk$State), "_formtemplate")]]
        extravalues <- str_subset(unique(unlist(pulledform)), "~HEADER~|~DATA~|~IGNORE~|", negate = TRUE)
        if(length(extravalues) > 0) {
          message <- paste("Additional values", paste(head(extravalues), collapse = ","), "must be designated as '~HEADER~', '~DATA~', or '~IGNORE~")
        stop(message)
          }
        subparts <- split_forms(dat_raw, pulledform)

        subparts_merge_ready <- munge_forms(subparts) %>% map(., ~if(nrow(.x) == 0) {add_row(.x)} else {.x})
        dat_rare <- reduce(subparts_merge_ready, merge, .dir = "forward")
        headercrosswalk <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FORM~", "", OldName)))
      } else {
        write.table(dat_raw, 
                    file = file.path(existingCrosswalks, "StateForms", paste0(unique(headercrosswalk$State), "_formtemplate.csv")),
                    row.names = FALSE, col.names = FALSE, sep = ",")
        message <- paste("Please use", paste0(unique(headercrosswalk$State), "_formtemplate.csv"), "to indicate '~HEADER~', '~DATA~', and '~IGNORE~' cells. " )
        stop(message)
      }
      } else {dat_rare <- dat_raw}

    if("~PIVOT~" %in% headercrosswalk$OldName) {
      pivot_instr <- updatedCrosswalks$DataPivots |> dplyr::filter(file %in% headercrosswalk$file)
      
      if(nrow(pivot_instr) > 0) {

        mutatecode <- with(pivot_instr,
                           paste0(
                             ifelse(grepl("=", names_tofrom), paste0('dplyr::mutate(., ', names_tofrom, ')'), "{.}"),
                             "%>%", ifelse(grepl("=", cols), paste0('dplyr::mutate(., ', cols, ')'), "{.}")))
        
        selectcode <- 
          with(pivot_instr,
               {
                 ifelse(long_wide == "wide",
                        {paste0('dplyr::select(., any_of(c("', paste(
                          ifelse(
                            any(grepl("=", c(names_tofrom, cols))), 
                            paste(stringr::str_trim(
                              unlist(stringr::str_extract_all(
                                unlist(stringr::str_split(
                                  stringr::str_remove(c(names_tofrom, cols), "=="),
                                  ",")),
                                ".*(?==)"))),
                            collapse = '", "'
                            ), 
                            paste(c(names_tofrom, cols), sep = '", "')),
                          values_tofrom, 
                          paste(headercrosswalk$OldName, collapse = '", "'), sep = '", "'), '")))')}, "{.}")})
        
        pivotcode <- 
          with(pivot_instr,
               ifelse(long_wide == "long",
                      paste0('tidyr::pivot_longer(.',
                             paste0(', cols = ', cols),
                             ', names_to = c(', paste0(
                               '"', paste0(stringr::str_trim(unlist(stringr::str_split(names_tofrom, ","))), collapse = '", "'), '"'
                             ), ')',
                             ifelse(grepl("sep:", names_pattern), 
                                    paste0(', names_sep = ', gsub("sep:", "", names_pattern)), ''),
                             ', values_to = "', values_tofrom, '"',
                             ifelse(values_transform == '', '', paste0(', values_transform = list(', values_transform, ')')), ')'),
                      paste0('tidyr::pivot_wider(.',
                             ', names_from = c("', ifelse(grepl("=", names_tofrom), stringr::str_trim(stringr::str_extract(names_tofrom, ".*(?==)")), names_tofrom), '")',
                             ', values_from = "', values_tofrom, '")')))
        
        filtercode <- with(pivot_instr, ifelse(long_wide == "long", paste0('dplyr::filter(., !is.na(', values_tofrom, '))'), '{.}'))
        dat_medrare <- suppressWarnings({
          dat_rare %>% 
            {eval(parse(text = mutatecode))} %>%
            {eval(parse(text = selectcode))} %>%
            {eval(parse(text = pivotcode))} %>%
            {eval(parse(text = filtercode))}
        })
 
        headercrosswalk <- headercrosswalk |> dplyr::mutate(OldName = dplyr::case_when(OldName == "~PIVOT~" ~ NewName,
                                                                          TRUE ~ OldName))
      } else {
        stop(
          paste0(
            "Pivot instructions need to be entered into DataPivots.csv for file ", 
            unique(headercrosswalk$file)))}
      
    } else {dat_medrare <- dat_rare}
    
    if(any(grepl("~FILL~", headercrosswalk$OldName))) {
      
      fillcols <- gsub("~FILL~", "", 
                        headercrosswalk$OldName[grepl("~FILL~", 
                                                      headercrosswalk$OldName)])
      dat_med <- dat_medrare %>%
        mutate(across(all_of(fillcols), ~zoo::na.locf(.)))
      headercrosswalk <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FILL~", "", OldName)))
    } else {dat_med <- dat_medrare}
    
    
    if(any(c("NewName", "OldName") %in% names(headercrosswalk))) {
      tmp <- suppressMessages(purrr::map2_dfc(headercrosswalk$NewName, headercrosswalk$OldName, ~{
        new <- .x
        old <- unlist(stringr::str_split(.y, ", "))
        
        purrr::map(old, ~{
            old_sub <- .x
            if(old_sub %in% names(dat_med)) {
              tmp <- tibble::tibble(!!new := dat_med[[old_sub]])} else {
                if(old_sub %in% names(dat_med)) {stop("Something went wrong in the pivots")} else 
                  if(!exists("mutatecode")) {
                    stop("Check entries in HeaderCrosswalk.csv that they match the data exactly.")
                  } else {
                    names_check <- dat_rare %>%
                      {eval(parse(text = mutatecode))} %>%
                      {eval(parse(text = selectcode))} %>%
                      {eval(parse(text = paste0("dplyr::select(., ", stringr::str_extract(pivotcode, "(?<=cols = ).*(?=, names_to)"), ")")))} |>
                      names()
                    nm <- manual_update(data.frame(tmp = NA), unique(headercrosswalk$file), old_sub, updatedCrosswalks, existingCrosswalks, names_check)
                    tmp <- tibble::tibble(!!new := nm[[old_sub]])}}
            tmp
          }) |> purrr::list_cbind(name_repair = "unique")
      }))
    } else {tmp <- data.frame()}
    tmp
  })
  names(dat) <- headers_classified$file
  
  return(dat)
}












