
shapefileextensions <- c(".shp", ".dbf", ".htm", ".prj", ".sbn", ".sbx", 
                         ".shp.xml", ".shx")

read_in_datafile <- function(datafp, fp) {
  data <- if(grepl("\\~\\$", fp)) {
    list("Temporary and/or corrupted file")
  } else if(
    grepl(".csv|.txt|.rdb", fp)) {
    read.csv(file.path(datafp, fp), fill = TRUE, header = FALSE)
  } else if(grepl(".xlsx|.xls", fp)) {
    workbook_fp <- str_extract(fp, ".*(?=\\$)")
    sheetnm <- str_extract(fp, "(?<=\\$).*")
    suppressWarnings(suppressMessages(
      readxl::read_excel(file.path(datafp, workbook_fp), sheet = sheetnm)))
  } else if(grepl(".docx", fp)) {
    dat <- officer::read_docx(file.path(datafp, fp))
    txt <- officer::docx_summary(dat)$text
    data.frame(text = txt)
  } else if (grepl(paste(shapefileextensions, collapse = "|"), fp)) {
    fp_shp <- gsub(paste(shapefileextensions, collapse = "|"), ".shp", fp)
    dat <- st_read(file.path(datafp, fp_shp))
  } else if (grepl(".pdf", fp)) {
    dat <- 
      imap_dfr(str_split(pdftools::pdf_text(pdf = file.path(datafp, fp)), "\n"), 
               ~{data.frame(text = .x) %>% mutate(page = .y)})
    data
  } else {browser()
    stop(paste0("New database type found that has not been built in yet (", fp, ")"))
  }
  data
}



readandrename_columns <- function(datafp, HeaderCrosswalk, pivots, HardCodes) {
  filledheader <- HeaderCrosswalk

  headers_classified <- filledheader %>% na.omit() %>% filter(State %in% state.abb)
  
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
                if(!exists("mutatecode")) {browser()
                  stop("Check entries in HeaderCrosswalk.csv that they match the data exactly.")
                  } else {
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












