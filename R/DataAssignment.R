
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
  usefulrows <- which(!rowSums(is.na(form_df) | form_df == "~IGNORE~") == length(form_df))
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
                        usefulcols <- colSums(is.na(.x) | .x == "~IGNORE~") == nrow(.x)
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

munge_forms <- function(dataformlist, filename) {
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
    } else if(.y == "transpose") {
      suppressWarnings({
        form_t <- t(.x$form) %>% as.data.frame() %>% janitor::row_to_names(1)})
      datarows_t <- which(rowSums(form_t == "~DATA~") > 0)
      t(.x$data) %>% as.data.frame() %>% janitor::row_to_names(1) %>% .[datarows_t,]
    } else {
          message <- paste("Format of", filename, "still needs to be handled in code. Please leave all headers as NA for now until code can accomodate.")
        stop(message)
          }
  })
}

checkformtype <- function(form) {
  
  form2 <- form %>% select_if(~!all(is.na(.)))
  
  type <- ifelse(janitor::find_header(form2) == 1 & (!"~DATA~" %in% unlist(form2[1,])) & 
                   (!"~HEADER~" %in% unlist(form2[-1,])),
                 "tidy",
                 ifelse(
                   janitor::find_header(form2) > 1 & 
                     (!"~DATA~" %in% unlist(form2[c(1:janitor::find_header(form2)),])) &
                     (!"~HEADER~" %in% unlist(form2[-c(1:janitor::find_header(form2)),])),
                   "compoundheader",
                   ifelse(
                     (!"~DATA~" %in% unlist(form2[,1])) & 
                       (!"~HEADER~" %in% unlist(form2[,-1])),
                     "transpose",
                     "TBD"
                   ))
                 )
  return(type)
}

applyFORMrules <- function(dat, headercrosswalk, updatedCrosswalks, existingCrosswalks) {
  dat_chr <- dat %>% mutate(across(everything(), ~as.character(.)))
  dat1 <- rbind(names(dat_chr), dat_chr) %>%
    dplyr::mutate(across(everything(), ~gsub("\\.\\.\\.[[:digit:]]+", NA_character_, .))) %>%
    dplyr::mutate(across(everything(), ~gsub("^$", NA_character_, .)))
  
  filename <- paste(unique(headercrosswalk$State),
                    paste(dim(dat1), collapse = "x"), 
                    "formtemplate", sep = "_")
  if(filename %in% names(updatedCrosswalks$Forms)) {
    
    pulledform <- updatedCrosswalks$Forms[[filename]]
    extravalues <- str_subset(unique(unlist(pulledform)), "~HEADER~|~DATA~|~IGNORE~|^$", negate = TRUE)
    if(length(extravalues) > 0) {
      message <- paste("Additional values", paste(head(extravalues), collapse = ","), 
                       "must be designated as '~HEADER~', '~DATA~', or '~IGNORE~ in ", filename)
      stop(message)
    }
    subparts <- split_forms(dat1, pulledform)
    
    fln <- unique(headercrosswalk$file)
    
    subparts_merge_ready <- munge_forms(subparts, fln) %>% map(., ~if(nrow(.x) == 0) {add_row(.x)} else {.x})
    dat2 <- reduce(subparts_merge_ready, merge, .dir = "forward")
    headercrosswalk <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FORM~", "", OldName)))
  } else {
    write.table(dat1, 
                file = file.path(existingCrosswalks, "StateForms", paste0(filename, ".csv")),
                row.names = FALSE, col.names = FALSE, sep = ",")
    message <- paste("Please use", filename, "to indicate '~HEADER~', '~DATA~', and '~IGNORE~' cells. " )
    stop(message)
  }
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk))
}

applyBLANKrules <- function(dat, headercrosswalk) {
  blanknames <- which(is.na(names(dat)))
  names(dat)[which(is.na(names(dat)))] <- paste0("V", blanknames)
  headercrosswalk$OldName[which(grepl("~BLANK~", headercrosswalk$OldName))] <- paste0("V", blanknames)
  return(list(dat_edit = dat, headercrosswalk = headercrosswalk))
}

applyFILLrules <- function(dat, headercrosswalk) {
  fillcols <- gsub("~FILL~", "", 
                   headercrosswalk$OldName[grepl("~FILL~", 
                                                 headercrosswalk$OldName)])
  dat2 <- dat %>%
    mutate(across(all_of(fillcols), ~zoo::na.locf(.)))
  headercrosswalk2 <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FILL~", "", OldName)))
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk2))
}

applyPIVOTrules <- function(dat, headercrosswalk, updatedCrosswalks) {
  pivot_instr <- updatedCrosswalks$DataPivots |> dplyr::filter(file %in% headercrosswalk$file)
  
  if(nrow(pivot_instr) > 0) {
    
    instructions <- map(transpose(pivot_instr), ~{
      mutatecode <- paste0(
                           ifelse(grepl("=", .x$names_tofrom), paste0('dplyr::mutate(., ', .x$names_tofrom, ')'), "{.}"),
                           "%>%", ifelse(grepl("=", .x$cols), paste0('dplyr::mutate(., ', .x$cols, ')'), "{.}"))
      selectcode <- ifelse(.x$long_wide == "wide",
                      paste0('dplyr::select(., any_of(c("', paste(
                        ifelse(
                          any(grepl("=", c(.x$names_tofrom, .x$cols))), 
                          paste(
                            str_subset(unique(
                              stringr::str_trim(
                            unlist(stringr::str_extract_all(
                              unlist(stringr::str_split(
                                stringr::str_remove(c(.x$names_tofrom, .x$cols), "=="),
                                ",")),
                              "[^=]*(?==|$)")))
                            ), "%|::|\\(|\"", negate = TRUE),
                            collapse = '", "'), 
                          paste(c(.x$names_tofrom, .x$cols), sep = '", "')),
                        .x$values_tofrom, 
                        paste(unique(headercrosswalk$OldName), collapse = '", "'), sep = '", "'), '")))'), "{.}")
      
      pivotcode <- ifelse(.x$long_wide == "long",
                    paste0('tidyr::pivot_longer(.',
                           paste0(', cols = ', .x$cols),
                           ', names_to = c(', paste0(
                             '"', paste0(stringr::str_trim(unlist(stringr::str_split(.x$names_tofrom, ","))), collapse = '", "'), '"'
                           ), ')',
                           ifelse(grepl("sep:", .x$names_pattern), 
                                  paste0(', names_sep = ', gsub("sep:", "", .x$names_pattern)), ''),
                           ', values_to = "', .x$values_tofrom, '"',
                           ifelse(.x$values_transform == '', '', paste0(', values_transform = list(', .x$values_transform, ')')), ')'),
                    paste0('tidyr::pivot_wider(.',
                           ', names_from = c("', ifelse(grepl("=", .x$names_tofrom), stringr::str_trim(stringr::str_extract(.x$names_tofrom, "[^=]*(?==)")), .x$names_tofrom), '")',
                           ', values_from = "', .x$values_tofrom, '")'))
      
      filtercode <- ifelse(.x$long_wide == "long", paste0('dplyr::filter(., !is.na(', .x$values_tofrom, '))'), '{.}')
      
      intr <- list(mutatecode = mutatecode, selectcode = selectcode, pivotcode = pivotcode, filtercode = filtercode)
      intr
    })

    dat2 <- suppressWarnings({dat %>% 
        {eval(parse(text = paste(unlist(instructions, use.names = FALSE), 
                                 collapse = " %>% ")))}
    })
    
    headercrosswalk2 <- headercrosswalk |> dplyr::mutate(OldName = dplyr::case_when(OldName == "~PIVOT~" ~ NewName,
                                                                                   TRUE ~ OldName))
  } else {
    stop(
      paste0(
        "Pivot instructions need to be entered into DataPivots.csv for file ", 
        unique(headercrosswalk$file)))}
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk2, 
              pivotinstructions = instructions))
}

readandrename_columns <- function(datafp, updatedCrosswalks, existingCrosswalks) {
  
  filledheader <- updatedCrosswalks$HeaderCrosswalk
  
  headers_classified <- filledheader |> na.omit() |> dplyr::filter(State %in% state.abb)
  
  dat <- purrr::imap(headers_classified$file, ~{
    i <- .y 
    dat_raw <- read_in_datafile(datafp, .x)
    keys <- c("State", "file", "IsReadMe")
    headercrosswalk <- headers_classified |>
      dplyr::slice(i) |> dplyr::select(where(~{. != ""})) %>% 
      {if(!all(names(.) %in% keys)) { 
        tidyr::pivot_longer(., cols = -any_of(keys), names_to = "NewName", 
                     values_to = "OldName")
      } else {.}}
    dat_edit <- dat_raw
    
    if(any(grepl("~FORM~", headercrosswalk$OldName))) {
      formapplied <- applyFORMrules(dat_edit, headercrosswalk, updatedCrosswalks, existingCrosswalks)
      dat_edit <- formapplied$dat_edit
      headercrosswalk <- formapplied$headercrosswalk
      }

    if("~PIVOT~" %in% headercrosswalk$OldName) {
      pivotapplied <- applyPIVOTrules(dat_edit, headercrosswalk, updatedCrosswalks)
      dat_edit <- pivotapplied$dat_edit
      headercrosswalk <- pivotapplied$headercrosswalk
    }
    
    if(any(grepl("~FILL~", headercrosswalk$OldName))) {
      fillapplied <- applyFILLrules(dat_edit, headercrosswalk)
      dat_edit <- fillapplied$dat_edit
      headercrosswalk <- fillapplied$headercrosswalk
    } 
    
    if(any(grepl("~BLANK~", headercrosswalk$OldName))) {
      blankapplied <- applyBLANKrules(dat_edit, headercrosswalk)
      dat_edit <- blankapplied$dat_edit
      headercrosswalk <- blankapplied$headercrosswalk
      }
    
    if(any(c("NewName", "OldName") %in% names(headercrosswalk))) {
      tmp <- suppressMessages(purrr::map2_dfc(headercrosswalk$NewName, headercrosswalk$OldName, ~{
        new <- .x
        old <- unlist(stringr::str_split(.y, ", "))
        
        purrr::map(old, ~{
            old_sub <- .x
            if(old_sub %in% names(dat_edit)) {
              tmp <- tibble::tibble(!!new := dat_edit[[old_sub]])} else {
                if(old_sub %in% names(dat_edit)) {
                  stop("Something went wrong in the pivots")
                  } else if(!exists("pivotapplied")) {
                    stop("Check entries in HeaderCrosswalk.csv that they match the data exactly.")
                  } else {
                    names_check <- dat_raw %>%
                      {eval(parse(text = flatten(pivotapplied$pivotinstructions)$mutatecode))} %>%
                      {eval(parse(text = flatten(pivotapplied$pivotinstructions)$selectcode))} %>%
                      {eval(parse(text = paste0("dplyr::select(., ", stringr::str_extract(flatten(pivotapplied$pivotinstructions)$pivotcode, "(?<=cols = ).*(?=, names_to)"), ")")))} |>
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












