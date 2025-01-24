# Title: Data Assignment Functions
# Description: This script contains utility functions for reading, reformatting, and renaming state data files.
# Author: Cathy Chamberlin (cchamberlin@usgs.gov) & Lily Gorman Sanisaca (lgormansanisaca@usgs.gov)
# Date: 8/22/24

# Define valid extensions for shapefiles
shapefileextensions <- c(".shp", ".dbf", ".htm", ".prj", ".sbn", ".sbx", 
                         ".shp.xml", ".shx")

#' Read in Data from Various File Formats
#'
#' This function reads data from various file formats including CSV, Excel, Word, 
#' shapefiles, and PDF. If the file is identified as temporary or corrupted, a 
#' message is returned instead of the data.
#'
#' @param datafp A character string representing the file path to the directory 
#' where the files are located.
#' @param fp A character string specifying the filename to be read, including its 
#' extension.
#'
#' @return A data frame containing the read data, or a message indicating that 
#' the file is temporary or corrupted.
#' 
#' @examples
#' # Assuming you have a CSV file "data.csv" in the "data" directory
#' df <- read_in_datafile(datafp = "data", fp = "data.csv")
#'
#' # Reading an Excel file "data.xlsx" with a specified sheet
#' df <- read_in_datafile(datafp = "data", fp = "data.xlsx$Sheet1")
#'
#' @export
read_in_datafile <- function(datafp, fp) {
  # Check if the file is a temporary or corrupted file
  data <- if(grepl("\\~\\$", fp)) {
    list("Temporary and/or corrupted file")  # Return a message if it's a temporary file
  } else if(
    grepl(".csv|.txt|.rdb", fp)) {
    # Read CSV or text files 
    data.table::fread(file.path(datafp, fp), fill = TRUE, header = TRUE, data.table = FALSE)
  } else if(grepl(".xlsx|.xls", fp)) {
    # Read Excel files
    workbook_fp <- stringr::str_extract(fp, ".*(?=\\$)")  # Extract workbook filename
    sheetnm <- stringr::str_extract(fp, "(?<=\\$).*")  # Extract sheet name
    suppressWarnings(suppressMessages(
      readxl::read_excel(file.path(datafp, workbook_fp), sheet = sheetnm)))  # Read worksheet
  } else if(grepl(".docx", fp)) {
    # Read Word documents
    dat <- officer::read_docx(file.path(datafp, fp))  # Load the Word document
    txt <- officer::docx_summary(dat)$text  # Extract text from the document
    data.frame(text = txt)  # Return text as a data frame
  } else if (grepl(paste(shapefileextensions, collapse = "|"), fp)) {
    # Read shapefiles
    fp_shp <- gsub(paste(shapefileextensions, collapse = "|"), ".shp", fp)  # Get the main shapefile
    dat <- sf::st_read(file.path(datafp, fp_shp), quiet = TRUE) %>%
      sf::st_drop_geometry()  # Read the spatial data and drop geometry
    dat  # Return the data
  } else if (grepl(".pdf", fp)) {
    # Read PDF files
    dat <- 
      purrr::imap_dfr(stringr::str_split(pdftools::pdf_text(pdf = file.path(datafp, fp)), "\n"), 
                      ~{data.frame(text = .x) |> dplyr::mutate(page = .y)})  # Extract text and page number
    dat  # Return the data
  } else {
    # Stop execution if the file format is unrecognized
    stop(paste0("New database type found that has not been built in yet (", fp, ")"))
  }
  # Clean up data by replacing empty strings with NA
  tmp <- data %>% 
    dplyr::mutate(dplyr::across(everything(), 
                  .fns = ~str_replace_all(str_trim(.), "^$", NA_character_)))
  return(tmp)  # Return the cleaned data
}

#' Split Forms and Data into Separate Components
#'
#' This function takes a data frame and a form data frame, aligning their 
#' structures and splitting them into separate components based on specific 
#' criteria, such as ignoring certain rows or identifying useful data.
#'
#' @param data A data frame containing the data to be split.
#' @param form_df A data frame that defines the forms, with certain markers 
#' indicating which rows or columns to ignore.
#'
#' @return A list containing two elements: 
#'         \item{data}{A list of data frames containing the split data.}
#'         \item{form}{A list of data frames containing the split forms.}
#' 
#' @details The function identifies useful rows in the form data that
#'          are not marked "~IGNORE~". It creates groups of contiguous 
#'          useful rows and then splits both the form and data into 
#'          subcomponents based on these groups. The resulting data frames 
#'          are cleaned according to the defined markers such as 
#'          "~HEADER~" and "~IGNORE~".
#'
#' @examples
#' # Example usage
#' form_example <- data.frame(A = c("~HEADER~", NA, "~DATA~"),
#'                            B = c("~IGNORE~", "~DATA~", NA))
#' data_example <- data.frame(A = c("value1", "value2", "value3"),
#'                            B = c(NA, "value5", "value6"))
#' result <- split_forms(data = data_example, form_df = form_example)
#'
#' @export
split_forms <- function(data, form_df) {
  # This function splits the data frames into smaller data frames based on entries into the form templates
  # Align data column names to those in the form data frame
  names(data) <- names(form_df)
  # Identify useful rows in the form data that are not marked for ignoring
  usefulrows <- which(!rowSums(is.na(form_df) | form_df == "~IGNORE~") == length(form_df))
  # Create a grouping variable for contiguous useful rows
  rowsplits <- c(0, cumsum(diff(usefulrows) > 1))
  
  # Split form data based on useful rows and their grouping
  forms_vsplit <- form_df %>% dplyr::slice(usefulrows) %>%
    dplyr::mutate(group = rowsplits) %>%
    dplyr::group_by(group) %>%
    dplyr::group_split(.keep = FALSE) 
 
   # Split the data based on the same useful rows and groupings
  data_vsplit <- data %>% dplyr::slice(usefulrows) %>%
    dplyr::mutate(group = rowsplits) %>%
    dplyr::group_by(group) %>%
    dplyr::group_split(.keep = FALSE)
  
  # Check for forms that don't contain the header marker
  if(any(map_lgl(forms_vsplit, ~!"~HEADER~" %in% unlist(.x)))) {
    vindex <- map(forms_vsplit, ~dplyr::mutate(
      .x, 
      dplyr::across(.cols = everything(), 
             .fns = ~case_when(. == "~IGNORE~" ~ NA_character_, TRUE ~ .)))) %>%
      map(., ~!is.na(.x)) %>%
      map(., ~{
        as.data.frame(.x) %>% map_lgl(., ~{sum(.x) > 0})
      }) %>%
      map(., ~which(.x))
    
    # Filter forms and data based on valid indices
    forms_vsplit <- map2(forms_vsplit, vindex, ~.x[.y])
    data_vsplit <- map2(data_vsplit, vindex, ~.x[.y])
    
    # Identify orphaned data (i.e. doesn't contain any header markers) and match data based on column counts
    orphandata <- which(map_lgl(forms_vsplit, ~!"~HEADER~" %in% unlist(.x)))
    
    matchdata <- which(map_lgl(forms_vsplit, ~ncol(.x) == ncol(forms_vsplit[[orphandata]]))[-orphandata])
    
    # Combine matched and orphaned forms
    forms_vsplit <- list(
      forms_vsplit[-c(matchdata, orphandata)], 
      dplyr::bind_rows(forms_vsplit[[matchdata]], forms_vsplit[[orphandata]])) %>% 
      list_flatten()
    
    # Combine corresponding data as well
    data_vsplit <- list(
      data_vsplit[-c(matchdata, orphandata)], 
      dplyr::bind_rows(data_vsplit[[matchdata]], data_vsplit[[orphandata]])) %>% 
      list_flatten()
  }
  
  # Determine column splits for each form based on NA or ignore markers
  hsplits <- purrr::map(forms_vsplit,
                        ~{subdf <- .x
                        usefulcols <- colSums(is.na(.x) | .x == "~IGNORE~") == nrow(.x)
                        colsplits <- c(0, cumsum(diff(usefulcols) > 1))
                        colsplits})
  
  # Further split forms and data based on identified column splits
  forms_vhsplit <- map2(forms_vsplit, hsplits, ~{
    x <- .x; y <- .y
    purrr::map(unique(y), ~{x[,which(y == .x)]})
  }) %>% list_flatten()
  
  data_vhsplit <- map2(data_vsplit, hsplits, ~{
    x <- .x; y <- .y
    purrr::map(unique(y), ~{x[,which(y == .x)]})
  }) %>% list_flatten()
  
  # Drop any remaining ~IGNORE~ columns 
  keepcols <- map(forms_vhsplit, 
      ~(which(unlist(dplyr::summarize(.x, dplyr::across(.cols = everything(), 
                                    .fns = ~(!all(. == "~IGNORE~")))), use.names = FALSE))))
  
  forms_split <- map2(forms_vhsplit, keepcols, ~(.x %>% dplyr::select(all_of(.y))))
  dat_split <- map2(data_vhsplit, keepcols, ~(.x %>% dplyr::select(all_of(.y))))
  
  # Determine which splits contain useful data
  usefulsplits <- map_lgl(forms_split, ~{any(c("~HEADER~", "~DATA~") %in% unique(unlist(.x)))})
  
  # Return a transposed list of useful data and corresponding forms
  return(list_transpose(list(data = dat_split[usefulsplits], form = forms_split[usefulsplits]), simplify = FALSE))
}

#' Process and Format Data from Various Forms
#'
#' This function takes a list of data forms and their respective metadata 
#' and processes them based on the identified format types. It transforms 
#' the data into a tidy format for further analysis.
#'
#' @param dataformlist A list containing data forms and their associated 
#'        metadata, with each element comprising a `form` and `data` 
#'        component.
#' @param filename A string representing the name of the file being processed, 
#'        used for informative messages.
#'
#' @return A list of data frames, each formatted based on its specified 
#'         format type, ready for further analysis or reporting.
#'
#' @details The function evaluates the format of each data form and processes 
#'          it accordingly. It currently supports "tidy", "compoundheader", 
#'          and "transpose" formats. If a format is unrecognized, it raises 
#'          an error prompting that further handling is needed.
#'
#' @examples
#' # Example usage
#' processed_forms <- munge_forms(dataformlist = my_data_list, filename = "datafile.csv")
#'
#' @export
munge_forms <- function(dataformlist, filename) {
  # Identify the format of each form in the list
  nativeformats <- map(dataformlist, ~{
    checkformtype(.x$form)
  })
  # Process each data form based on its identified format
  map2(dataformlist, nativeformats, ~{
    if(.y == "tidy") {
      # Handle tidy format by extracting header and cleaning data
      .x$data[,which(.x$form[1,] == "~HEADER~")] %>%
        janitor::remove_empty("rows") %>%
        janitor::row_to_names(1)
    } else if(.y == "compoundheader") {
      # Handle compound header format
      headers <- .x$data[c(1, find_header(.x$data)),] %>% map(., ~paste(na.omit(unique(.x)), collapse = "_"))
      data <- .x$data[-c(1, find_header(.x$data)),]
      
      rbind(headers, data) %>% janitor::remove_empty("rows") %>%
        janitor::row_to_names(1)
    } else if(.y == "transpose") {
      # Handle transposed data format
      suppressWarnings({
        form_t <- t(.x$form) %>% as.data.frame() %>%
          janitor::remove_empty("rows") %>% janitor::row_to_names(1)})
      datarows_t <- which(rowSums(form_t == "~DATA~") > 0)
      suppressMessages({data_t <- t(.x$data) %>% as.data.frame() %>%
          janitor::remove_empty("rows") %>% janitor::row_to_names(1) %>%
          as_tibble(.name_repair = "unique") %>% slice(., datarows_t)})
      data_t
    } else {
      # Stop execution if the format is unknown
          message <- paste("Format of", filename, "still needs to be handled in code. Please leave all headers as NA for now until code can accomodate.")
      stop(message)
    }
  })
}

#' Determine the Format Type of a Given Form
#'
#' This function analyzes a given data form and determines its format type 
#' based on the structure and content of the data.
#'
#' @param form A data frame representing the data form to be analyzed.
#'
#' @return A string indicating the format type of the form. Possible 
#'         return values include "tidy", "compoundheader", "transpose", 
#'         or "TBD" (to be determined).
#'
#' @details The function removes any columns from the form where all 
#'          elements are NA and assesses various conditions to classify 
#'          the format of the data form. These conditions help in distinguishing 
#'          between different formats used in data analysis.
#'
#' @examples
#' # Example usage
#' form_type <- checkformtype(my_form)
#'
#' @export
checkformtype <- function(form) {
  
  # Remove columns where all elements are NA
  form2 <- form %>% dplyr::select_if(~!all(is.na(.))) 
  
  # Determine the type of form based on specific conditions
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
  # Return the determined type
  return(type)
}

#' Apply FORM Rules and Clean Data Based on Specified Crosswalks
#'
#' This function processes a dataset according to specific FORM rules 
#' and applies crosswalks for cleaning and organizing the data.
#'
#' @param dat A data frame containing the data to be processed.
#' @param headercrosswalk A data frame that maps headers across formats.
#' @param updatedCrosswalks A list of updated crosswalks for reference.
#' @param existingCrosswalks A path to existing crosswalk directories.
#'
#' @return A list containing the cleaned dataset and 
#'         an updated header crosswalk.
#'
#' @details The function converts all dataset columns to character types, 
#'          cleans the data by removing unwanted patterns, and checks against 
#'          updated form crosswalks. If the dataset corresponds to an existing 
#'          format, it merges and cleans parts accordingly; otherwise, it 
#'          prompts the user for necessary categorization and writes to CSV.
#'
#' @examples
#' # Example usage
#' result <- applyFORMrules(data_df, header_map, updated_mappings, existing_path)
#'
#' @export
applyFORMrules <- function(dat, headercrosswalk, updatedCrosswalks, existingCrosswalks) {
  # Convert all columns to character type
  # headercrosswalk$file gives the name of the file that may have failed
  dat_chr <- dat %>% dplyr::mutate(dplyr::across(everything(), ~as.character(.)))
  # Add column names as the first row and clean the data
  dat1 <- rbind(names(dat_chr), dat_chr) %>%
    dplyr::mutate(dplyr::across(everything(), ~gsub("\\.\\.\\.[[:digit:]]+", NA_character_, .))) %>%
    dplyr::mutate(dplyr::across(everything(), ~gsub("^$", NA_character_, .)))
  
  # Create a filename based on state and dimensions of the data
  filename <- paste(unique(headercrosswalk$State),
                    paste(dim(dat1), collapse = "x"), 
                    "formtemplate", sep = "_")
  # Check if the filename exists in the updated crosswalks
  if(filename %in% names(updatedCrosswalks$Forms)) {
    
    pulledform <- updatedCrosswalks$Forms[[filename]]
    extravalues <- str_subset(unique(unlist(pulledform)), "~HEADER~|~DATA~|~IGNORE~|^$", negate = TRUE)
    # Check for additional values that need to be categorized
    if(length(extravalues) > 0) {
      message <- paste("Additional values", paste(head(extravalues), collapse = ","), 
                       "must be designated as '~HEADER~', '~DATA~', or '~IGNORE~ in", filename)
      stop(message)
    }
    # Split forms and prepare for merging
    subparts <- split_forms(dat1, pulledform)
    
    fln <- unique(headercrosswalk$file)
    
    # Clean and prepare subparts for merging
    subparts_merge_ready <- munge_forms(subparts, fln) %>% map(., ~if(nrow(.x) == 0) {add_row(.x)} else {.x})
    # Merge the cleaned data parts
    dat2 <- reduce(subparts_merge_ready, merge, .dir = "forward")
    headercrosswalk <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FORM~", "", OldName)))
  } else {
    # Write the data to a CSV file if the filename does not exist
    write.table(dat1, 
                file = file.path(existingCrosswalks, "StateForms", paste0(filename, ".csv")),
                row.names = FALSE, col.names = FALSE, sep = ",")
    message <- paste("Please use", filename, "to indicate '~HEADER~', '~DATA~', and '~IGNORE~' cells.")
    stop(message)
  }
  # Return the edited data and updated header crosswalk
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk))
}

#' Apply Rules for Handling Blank Names in the Dataset
#'
#' This function identifies and renames columns in a dataset that have 
#' 'blank' names, defined as those containing three or more punctuation marks 
#' followed by a digit once the data are read in. It also updates the header crosswalk accordingly.
#'
#' @param dat A data frame whose column names are to be checked and potentially renamed.
#' @param headercrosswalk A data frame that maps the old names to new names.
#'
#' @return A list containing the cleaned dataset and the updated header 
#'         crosswalk.
#'
#' @details The function searches for column names that match a specific regex pattern 
#'          for blank names and renames these columns with a new format. It also 
#'          updates the `OldName` field in the header crosswalk where applicable.
#'
#' @examples
#' # Example usage
#' result <- applyBLANKrules(data_df, header_map)
#'
#' @export
applyBLANKrules <- function(dat, headercrosswalk) {
  # Identify columns with names such as ...1 or ...14
  blanknames <- which(str_detect(names(dat), "[[:punct:]]{3}(?=[[:digit:]])"))
  # Rename those columns with a new format
  names(dat)[blanknames] <- paste0("V", blanknames)
  # If dat is a completely empty data frame, which happens sometimes, e.g. when information is included in a picture of screenshotted text,
  # Then create a blank data frame
  if(all(dim(dat) == 0)) {
    dat <- data.frame(V = integer())
  }
  
  # Update OldName in the header crosswalk for any that include "~BLANK~"
  headercrosswalk$OldName[which(grepl("~BLANK~", headercrosswalk$OldName))] <- paste0("V", blanknames)
  # Return the edited data and updated header crosswalk
  return(list(dat_edit = dat, headercrosswalk = headercrosswalk))
}

#' Apply Rules for Filling Missing Values in Specific Columns
#'
#' This function identifies specified columns in a dataset that require filling 
#' of missing values using a "last observation carried forward" method. It also 
#' updates the header crosswalk by removing the "~FILL~" tag.
#'
#' @param dat A data frame with potential missing values in specific columns.
#' @param headercrosswalk A data frame that maps the old names of columns, some of which 
#'        may include the "~FILL~" tag.
#'
#' @return A list containing the modified dataset with filled missing 
#'         values and the updated header crosswalk 
#'
#' @details The function searches for columns marked with the "~FILL~" tag to determine 
#'          which columns should have their missing values filled. It applies the 
#'          `na.locf` function from the `zoo` package to achieve this and then removes 
#'          the "~FILL~" tag from the names in the header crosswalk.
#'
#' @examples
#' # Example usage
#' result <- applyFILLrules(data_df, header_map)
#'
#' @export
applyFILLrules <- function(dat, headercrosswalk) {
  # Identify columns that need to be filled, removing the "~FILL~" tag
  fillcols <- gsub("~FILL~", "", 
                   headercrosswalk$OldName[grepl("~FILL~", 
                                                 headercrosswalk$OldName)])
  # Fill missing values in the identified columns using last observation carried forward
  dat2 <- dat %>%
    dplyr::mutate(dplyr::across(all_of(fillcols), ~zoo::na.locf(., na.rm = FALSE)))
  # Update the header crosswalk to remove the "~FILL~" tag
  headercrosswalk2 <- headercrosswalk |> dplyr::mutate(OldName = str_trim(gsub("~FILL~", "", OldName)))
  # Return the edited data and updated header crosswalk
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk2))
}

#' This function applies pivot transformations to a dataset according to specified 
#' instructions in the pivot crosswalk. It modifies the dataset's structure 
#' based on the defined pivot actions and updates the header crosswalk.
#'
#' @param dat A data frame to be transformed based on pivot instructions.
#' @param headercrosswalk A data frame mapping column names, some of which may 
#'        require pivot transformations.
#' @param updatedCrosswalks A data frame containing the pivot instructions.
#'
#' @return A list containing:
#'         - `dat_edit`: The transformed dataset.
#'         - `headercrosswalk2`: The updated header crosswalk.
#'         - `pivotinstructions`: The instructions used for the pivot process.
#'
#' @details This function extracts pivot instructions for relevant files, constructs 
#'          the necessary code for mutations, selections, and filtering, and then 
#'          evaluates this code on the provided dataset. It ensures that any 
#'          transformations are appropriately applied.
#'
#' @examples
#' # Example usage
#' result <- applyPIVOTrules(data_df, header_map, updated_crosswalks)
#'
#' @export
applyPIVOTrules <- function(dat, headercrosswalk, updatedCrosswalks) {
  # Get the pivot instructions for the relevant file from the updated crosswalks
  pivot_instr <- updatedCrosswalks$DataPivots |> dplyr::filter(file %in% headercrosswalk$file)
  # pivot_instr$file gives the name of the file that may have caused a failure
  if(nrow(pivot_instr) > 0) {
    
    instructions <- map(purrr::transpose(pivot_instr), ~{

      # Create mutate code for the specified transformations
      mutatecode <- paste0(
        ifelse(grepl("=", .x$names_tofrom), paste0('dplyr::mutate(., ', .x$names_tofrom, ')'), "{.}"),
        "%>%", ifelse(grepl("=", .x$cols), paste0('dplyr::mutate(., ', .x$cols, ')'), "{.}"))
      # Create select code based on whether the pivot is to a wide or long format
      # This code is based on the inputs from the pivot instructions crosswalk
      # It can be rather finicky. If there are errors popping up in this function, the select code is a good place to check first.
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
      
      # Create pivot code based on whether transforming to long or wide format
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
                                 ', values_from = "', .x$values_tofrom, '"',
                                 ifelse(.x$values_transform == '', ')', 
                                        paste0(', values_fn = ', .x$values_transform, ')'))))
      
      # Create filter code based on whether transforming to long or wide format
      filtercode <- ifelse(.x$long_wide == "long", paste0('dplyr::filter(., !is.na(', .x$values_tofrom, '))'), '{.}')
      
      # Return all generated codes as a list
      intr <- list(mutatecode = mutatecode, selectcode = selectcode, pivotcode = pivotcode, filtercode = filtercode)
      intr
    })

    # Evaluate the constructed transformation instructions on the dataset
    dat2 <- suppressWarnings({dat %>% 
        {eval(parse(text = paste(unlist(instructions, use.names = FALSE), 
                                 collapse = " %>% unique(.) %>% ")))}})
    
    # Update the header crosswalk to remove "~PIVOT~"
    headercrosswalk2 <- headercrosswalk |> dplyr::mutate(OldName = dplyr::case_when(OldName == "~PIVOT~" ~ NewName,
                                               TRUE ~ OldName))
  } else {
    stop(
      paste0(
        "Pivot instructions need to be entered into DataPivots.csv for file ", 
        unique(headercrosswalk$file)))}
  # Return the edited data, updated header crosswalk, and pivot instructions
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk2, 
              pivotinstructions = instructions))
}

applyFILENAMErules <- function(dat, headercrosswalk) {
  # Identify columns that need to be filled with information from the file name
  filenamecols <- headercrosswalk$NewName[grepl("~FILENAME~", 
                                                 headercrosswalk$OldName)]
  # Fill missing values in the identified columns using last observation carried forward
  filename <- unique(headercrosswalk$file)
  
  newcols <- set_names(rep(filename, length(filenamecols)), filenamecols)
  dat2 <- dat %>%
    add_column(!!!newcols)
  # Update the header crosswalk to remove the "~FILL~" tag
  headercrosswalk2 <- headercrosswalk |> 
    dplyr::mutate(OldName = case_when(grepl("~FILENAME~", OldName) ~ NewName,
                                      !grepl("~FILENAME~", OldName) ~ OldName))
  # Return the edited data and updated header crosswalk
  return(list(dat_edit = dat2, headercrosswalk = headercrosswalk2))
  
}

#' Read and Rename Columns Based on Crosswalks
#'
#' This function reads data files and renames their columns based on specified
#' crosswalks. It includes rules for applying forms, pivoting, filling, and blanking.
#'
#' @param datafp A character vector of file paths to the data files.
#' @param updatedCrosswalks A list containing updated crosswalk information with a column named 'HeaderCrosswalk'.
#' @param existingCrosswalks A string of a file path for existing crosswalks for compatibility.
#' @param data A character vector that indicates the type of data being processed. 
#'             It should be either "State" or "National". 
#'
#' @return A list of data frames where each element corresponds to a processed data file,
#'         with columns renamed according to the crosswalks.
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   results <- readandrename_columns(datafp = "path/to/datadir",
#'                                     updatedCrosswalks = your_updated_crosswalks_list,
#'                                     existingCrosswalks = your_existing_crosswalks_fp,
#'                                     data = "State")
#'   }
#'
#' @export
readandrename_columns <- function(datafp, updatedCrosswalks, existingCrosswalks, data = c("State", "National")) {
  # If a file has been removed on disk, this function gives a NULL, which will cause errors further down the pipeline.
  # Suggest introducing a check for this in future updates.
  filledheader <- updatedCrosswalks$HeaderCrosswalk
  
  if(data == "State") {
    headers_classified <- filledheader |> na.omit() |> dplyr::filter(State %in% fedmatch::State_FIPS$Abbreviation)
  } else if(data == "National") {
    headers_classified <- filledheader |> na.omit() |> 
      dplyr::filter(gsub("\\$.*", "", file) %in% basename(unlist(datafp)))
  }
  
  dat <- purrr::imap(headers_classified$file, ~{
    i <- .y 
    if(data == "National") {
      filename <- gsub("\\$.*", "", .x)
      datafp <- dirname(datafp[[grep(filename, datafp)]])
    }
    dat_raw <- read_in_datafile(datafp, .x)
    keys <- c("State", "file", "IsReadMe")
    headercrosswalk <- headers_classified |>
      dplyr::slice(i) |> dplyr::select(where(~{. != ""})) %>% 
      {if(!all(names(.) %in% keys)) { 
        tidyr::pivot_longer(., cols = -any_of(keys), names_to = "NewName", 
                            values_to = "OldName")
      } else {.}}
    dat_edit <- dat_raw
    
    # Apply FORM rules if specified
    if(any(grepl("~FORM~", headercrosswalk$OldName))) {
      formapplied <- applyFORMrules(dat_edit, headercrosswalk, updatedCrosswalks, existingCrosswalks)
      dat_edit <- formapplied$dat_edit
      headercrosswalk <- formapplied$headercrosswalk
    }
    
    # Apply PIVOT rules if specified
    if("~PIVOT~" %in% headercrosswalk$OldName) {
      pivotapplied <- applyPIVOTrules(dat_edit, headercrosswalk, updatedCrosswalks)
      dat_edit <- pivotapplied$dat_edit
      headercrosswalk <- pivotapplied$headercrosswalk
    }
    
    # Apply FILL rules if specified
    if(any(grepl("~FILL~", headercrosswalk$OldName))) {
      fillapplied <- applyFILLrules(dat_edit, headercrosswalk)
      dat_edit <- fillapplied$dat_edit
      headercrosswalk <- fillapplied$headercrosswalk
    } 
    
    # Apply BLANK rules if specified
    if(any(grepl("~BLANK~", headercrosswalk$OldName))) {
      blankapplied <- applyBLANKrules(dat_edit, headercrosswalk)
      dat_edit <- blankapplied$dat_edit
      headercrosswalk <- blankapplied$headercrosswalk
    }
    
    # Apply FILENAME rules if specified
    if(any(grepl("~FILENAME~", headercrosswalk$OldName))) {
      filenameapplied <- applyFILENAMErules(dat_edit, headercrosswalk)
      dat_edit <- filenameapplied$dat_edit
      headercrosswalk <- filenameapplied$headercrosswalk
    }
    
    # Rename columns based on the specified old and new names
    if(any(c("NewName", "OldName") %in% names(headercrosswalk))) {
      tmp <- suppressMessages(purrr::map2_dfc(headercrosswalk$NewName, headercrosswalk$OldName, ~{
        new <- .x
        if(.y %in% names(dat_edit)) {old <- .y} else if(
          gsub("`", "", .y) %in% names(dat_edit)) {
          old <- gsub("`", "", .y) %in% names(dat_edit)} else if(
            (str_count(.y, "`") > 0) & (str_count(.y, "`") %% 2 == 0)
          ) {old <- gsub("`", "", .y)} else {
          old <- unlist(stringr::str_split(.y, ", "))}
        
        purrr::map(old, ~{
          old_sub <- .x
          if(old_sub %in% names(dat_edit)) {
            tmp <- tibble::tibble(!!new := dat_edit[[old_sub]])} else {
            if(old_sub %in% names(dat_edit)) {
              stop("Something went wrong in the pivots")
            } else if(!exists("pivotapplied")) {
              stop(paste0("Check entries ", old, " in HeaderCrosswalk.csv for file ",
              unique(headercrosswalk$file), " that they match the data exactly. Options include ", paste(names(dat_edit), collapse = ", ")))
            } else {
              if(is.na(stringr::str_extract(flatten(
                pivotapplied$pivotinstructions)$pivotcode, 
                "(?<=cols = ).*(?=, names_to)"))) {
                select_code <- "{.}"
              } else {
                select_code <- paste0(
                  "dplyr::select(., ",
                  stringr::str_extract(flatten(
                    pivotapplied$pivotinstructions)$pivotcode, 
                    "(?<=cols = ).*(?=, names_to)"), ")")
              }
              names_check <- dat_raw %>%
                {eval(parse(text = flatten(
                  pivotapplied$pivotinstructions)$mutatecode))} %>%
                {eval(parse(text = flatten(
                  pivotapplied$pivotinstructions)$selectcode))} %>%
                {eval(parse(text = select_code))} |>
                names()
              nm <- manual_update(data.frame(tmp = NA), 
                                  unique(headercrosswalk$file), new, 
                                  updatedCrosswalks, existingCrosswalks, 
                                  names_check)
              tmp <- tibble::tibble(!!new := nm[[new]])}
                }
            tmp
          }) |> purrr::list_cbind(name_repair = "unique")
      }))
    } else {tmp <- data.frame()}
    tmp
  })
  names(dat) <- headers_classified$file
  
  return(dat)
}












