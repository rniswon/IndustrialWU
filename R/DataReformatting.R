# Title: Data Cleaning Functions
# Description: This script contains utility functions for reformatting, cleaning, and standardizing data
# Author: Cathy Chamberlin (cchamberlin@usgs.gov) & Lily Gorman Sanisaca (lgormansanisaca@usgs.gov)
# Date: 8/22/24

#' Detect if a filename corresponds to a README entry
#'
#' This function checks if a given filename has an associated README entry
#' in the provided updated crosswalks data.
#'
#' @param filename A character string representing the name of the file to check.
#' @param updatedCrosswalks A list containing crosswalk information, 
#'                          including a 'HeaderCrosswalk' dataframe
#'
#' @return A logical value indicating whether the file has a corresponding README entry.
#'         Returns TRUE if it does, and FALSE otherwise.
#'
#' @examples
#' \dontrun{
#'   is_readme <- detect_readme("example_file.csv", updatedCrosswalks)
#'   }
#'
detect_readme <- function(filename, updatedCrosswalks) {
  # Extract the README entry corresponding to the provided filename from the crosswalks
  ReadMeentry <- updatedCrosswalks$HeaderCrosswalk %>% filter(file == filename) %>%  # Filter for the specific file
    pull(IsReadMe)  # Extract the IsReadMe column
  # Check if the README entry is not empty and return the result as a logical value
  ReadMeentry != ""
}

#' Convert DMS (Degrees, Minutes, Seconds) to Decimal Degrees
#'
#' This function converts a coordinate in DMS format (as a numeric string)
#' into decimal degrees. It assumes the input format is a string where 
#' the first two characters represent degrees, the next two represent minutes, 
#' and the remaining characters represent seconds.
#'
#' @param x A numeric string representing the DMS coordinate (e.g., "123456").
#'
#' @return A numeric value representing the coordinate in decimal degrees, 
#'         rounded to six decimal places.
#'
#' @examples
#' \dontrun{
#'   decimal_degrees <- convert2decimal("123456")
#'   }
#'
convert2decimal <- function(x) {
  # Remove leading zero from the input string (if present)
  x <- gsub("^0", "", as.character(x))
  # Extract degrees (first two characters) and convert to numeric
  degrees <- suppressWarnings(as.numeric(stringr::str_sub(x, 1, 2)))
  # Extract minutes (next two characters) and convert to numeric
  minutes <- suppressWarnings(as.numeric(stringr::str_sub(x, 3, 4)))
  # Extract seconds (remaining characters) and convert to numeric
  seconds <- suppressWarnings(as.numeric(stringr::str_sub(x, 5, nchar(x))))
  
  # Calculate decimal degrees and round to six decimal places
  round(degrees + (minutes + (seconds / 60)) / 60, 6)
}

#' Handle Coordinate Data in a Data Frame
#'
#' This function processes coordinate columns in a data frame, replacing 0 and NULL 
#' values with NA and converting coordinates given in certain formats into 
#' decimal degrees using the `convert2decimal` function.
#'
#' @param data A data frame containing coordinate data.
#' @param header A character string representing the header names related to coordinates.
#'
#' @return A data frame with modified coordinate columns where:
#'         - "0" and "NULL" are replaced with NA.
#'         - Values matching a specific format are converted to decimal degrees.
#'         - Other values are kept as character strings.
#'
#' @examples
#' \dontrun{
#'   cleaned_data <- handle_coordinates(data, "coordinate_header")
#'   }
#'
handle_coordinates <- function(data, header) {
  # Mutate the data frame to handle specified columns containing coordinates
  tmp <- data |>
    dplyr::mutate(dplyr::across(contains(header), ~{
      dplyr::case_when(
        . == "0" ~ NA_character_,  # Replace "0" with NA
        . == "NULL" ~ NA_character_,  # Replace "NULL" with NA
        stringr::str_detect(., "^[[:digit:]]{2}\\.") ~ as.character(.),  # Keep values with two digits followed by a dot as is
        stringr::str_detect(., "^[[:digit:]]{6}\\.?") ~ as.character(convert2decimal(.)),  # Convert DMS to decimal degrees
        .default = as.character(.)  # Convert all other values to character
      )
    })) 
  
  # Return the modified data frame
  tmp
}


#' Merge Two Data Frames and Replace NA Values
#'
#' This function merges two data frames, selecting complete rows without NA from both. 
#' It also groups the second data frame by specific variables, allowing for the 
#' summarization of unique values. If the grouping results in more rows than the number 
#' of merge variables, unique values will be retained in the final output.
#'
#' @param x A data frame to be merged, which should be complete without NA values in 
#'          the variables needed to merge.
#' @param y A second data frame to be merged, which will also be processed to remove 
#'          NA values and may be summarized.
#'
#' @return A merged data frame combining x and y, with NA values replaced and 
#'         appropriate summarization applied when necessary.
#'
#' @examples
#' \dontrun{
#'   result <- merge_andreplaceNA(data_frame1, data_frame2)
#'   }
#'
merge_andreplaceNA <- function(x, y) {
  
  # Select columns from x that have no NA values
  x_complete <- x |> dplyr::select(where(~!any(is.na(.))))
  # Select columns from y that have no NA values
  y_complete <- y |> dplyr::select(where(~!any(is.na(.))))
  
  # Identify common columns to use for merging
  merge_vars <- names(x_complete)[names(x_complete) %in% names(y_complete)]
  
  # Check if the maximum row count from grouped y exceeds the number of merge variables
  if(
    max(dplyr::pull(
      dplyr::summarize(dplyr::group_by(y, dplyr::across(all_of(merge_vars))), n = dplyr::n(), .groups = "drop"), 
      n)) > length(merge_vars)) {
    # Define data variables to be used in grouping. These cannot be summarized by group and must remain independent
    datavars <- c("Annual_reported", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Year", "Category")
    
    # Group y by merge variables and data variables, summarizing unique values
    y_unique <- y |> 
      dplyr::group_by(dplyr::across(any_of(c(merge_vars, datavars)))) |> 
      dplyr::summarize(dplyr::across(.cols = everything(),  .fns = ~paste(unique(.), collapse = ", ")), .groups = "drop") |>
      # If there are multiple locations or sources, keep only the first one
      dplyr::mutate(dplyr::across(any_of(c("Lat", "Lon", "SourceNumber")),  ~gsub(",.*", "", .))) ## If more than one location or source, keep only the first one. Source will only treated this way if it isn't used to merge - so if the other data is only to the facility level, not the source level.
  } else {y_unique <- y} # If  maximum row count from grouped y does not exceed the number of merge variables, keep y as is
  
  # Perform a full natural join on x and the unique version of y
  merge <- rquery::natural_join(x, y_unique, by = merge_vars, jointype = "FULL")
  
  # Return the merged data frame
  return(merge)
}


#' Add State Information to Data Frames
#'
#' This function extracts state abbreviations from a given input data structure 
#' and adds them as a new column named "State" to each data frame. The state 
#' abbreviation is extracted from the provided file or string format in the input.
#'
#' @param renamed_rawdat A list or data frame where each element contains data 
#'                       from which to extract the state information.
#'
#' @return A list of data frames with an additional "State" column added 
#'         containing the extracted state abbreviations.
#'
#' @examples
#' \dontrun{
#'   result <- add_state(renamed_data_list)
#'   }
#'
add_state <- function(renamed_rawdat) {
  # Iterate through each data frame in renamed_rawdat and add state information
  purrr::imap(renamed_rawdat, ~{
    # Extract the state abbreviation from the string using a regex pattern
    state <- stringr::str_extract(.y, "(?<=^/)[[:alpha:]]{2}")
    # Mutate the data frame to include the new "State" column
    .x |> dplyr::mutate(State = state)
  })
}

#' Concatenate Two Vectors with Commas
#'
#' This function takes two vectors, replaces occurrences of "NULL" with NA, 
#' and concatenates unique, non-NA values from both vectors into a single 
#' string separated by commas. If there are no unique values, it returns NA.
#'
#' @param x A character vector from which to concatenate values.
#' @param y A second character vector from which to concatenate values.
#'
#' @return A character vector containing the concatenated unique values from 
#'         both input vectors, or NA if no unique values are found.
#'
#' @examples
#' \dontrun{
#'   result <- pastecomma(c("value1", "NULL", "value2"), c("value2", "value3"))
#'   }
#'
pastecomma <- function(x, y) {
  # Replace occurrences of "NULL" with NA in both vectors
  x = gsub("NULL", NA_character_, x)
  y = gsub("NULL", NA_character_, y)
  # Concatenate unique non-NA values from both vectors, separated by commas
  concat <- unlist(map2(x, y, ~{paste(unique(na.omit(c(.x, .y))), collapse = ", ")}) %>% modify_if(~length(.) == 0, ~NA_character_), use.names = FALSE)
  # Return the concatenated result
  return(concat)
}

#' Concatenate Columns in a Data Frame
#'
#' This function concatenates specified columns in a data frame into a single 
#' column. It uses the `pastecomma` function to merge unique, non-NA values 
#' from the selected columns. The original columns are removed, and the new 
#' concatenated column is added back to the data frame.
#'
#' @param data A data frame from which to concatenate specified columns.
#' @param Column A character string specifying the column names to concatenate.
#'
#' @return A data frame with the specified columns concatenated into a single 
#'         column while removing the original columns.
#'
#' @examples
#' \dontrun{
#'   result <- concat_columns(data_frame, "targetColumn")
#'   }
#'
concat_columns <- function(data, Column) {
  # Select specified columns and concatenate them using pastecomma
  tmp <- data |>
    dplyr::select(contains(Column)) |> 
    dplyr::mutate(tmp = reduce(across(contains(Column)), .f = pastecomma)) |>
    dplyr::pull(tmp)
  # Remove original columns and add the concatenated column
  tmp2 <- data |>
    dplyr::select(-contains(Column)) |> dplyr::mutate(!!Column := tmp)
  # Return the modified data frame
  return(tmp2)
}

#' Handle Readme Files in Data Processing
#'
#' This function processes data related to readme files. It extracts relevant 
#' information from the specified header and passes it along with other parameters 
#' to another function (`handle_oddformats`) for further handling.
#'
#' @param data A data frame containing information, including the readme files.
#' @param fp A file path or identifier related to the data being processed.
#' @param header A character string specifying the header from which to extract 
#'               information.
#' @param updatedCrosswalks A list of updated crosswalk mappings.
#' @param existingCrosswalks A filepath for existing crosswalk mappings.
#'
#' @return The function returns the output from the `handle_oddformats` function.
#'
#' @examples
#' \dontrun{
#'   result <- handle_readmes(data_frame, "file_path", "header_name", updated_crosswalk_data, existing_crosswalk_data)
#'   }
#'
handle_readmes <- function(data, fp, header, updatedCrosswalks, existingCrosswalks) {
  # Extract information from the specified header in the data frame
  info <- unlist(data[[header]])
  # Pass the extracted information and other parameters to handle_oddformats
  tmp <- handle_oddformats(data, fp, header, updatedCrosswalks, existingCrosswalks, info)
  # Return the result for further use
  return(tmp)
}

#' Handle Header Mappings in Data Processing
#'
#' This function retrieves old header mappings from an updated crosswalks data set 
#' based on the provided file path. It then processes the data using another 
#' function (`handle_oddformats`) with the old header information.
#'
#' @param data A data frame containing the data to be processed.
#' @param fp A file path or identifier related to the data being processed.
#' @param header A character string specifying the header from which to extract 
#'               old mappings.
#' @param updatedCrosswalks A list of updated crosswalk mappings.
#' @param existingCrosswalks A file path for existing crosswalk mappings.
#'
#' @return The function returns the output from the `handle_oddformats` function 
#'         after processing with the old header.
#'
#' @examples
#' \dontrun{
#'   result <- handle_headers(data_frame, "file_path", "header_name", updated_crosswalk_data, existing_crosswalk_data)
#'   }
#'
handle_headers <- function(data, fp, header, updatedCrosswalks, existingCrosswalks) {
  # Retrieve the old header from the updated crosswalks for the specified file path
  oldheader <- updatedCrosswalks$HeaderCrosswalk |> dplyr::filter(file == fp) |> dplyr::pull(header)
  # Pass the old header information to handle_oddformats for further processing
  tmp <- handle_oddformats(data, fp, header, updatedCrosswalks, existingCrosswalks, info = oldheader)
  # Return the processed result
  return(tmp)
}

#' Handle Odd Formats in Data Processing
#'
#' This function processes data that may have unusual formats by calling a 
#' manual update function. It serves as an intermediary function to prepare data 
#' for specific handling based on the provided inputs.
#'
#' @param data A data frame containing the data to be processed.
#' @param fp A file path or identifier related to the data being processed.
#' @param header A character string specifying the header for processing.
#' @param updatedCrosswalks A list of updated crosswalk mappings.
#' @param existingCrosswalks A file path of existing crosswalk mappings.
#' @param info Additional information or metadata for processing.
#'
#' @return The function returns the output from the `manual_update` function after 
#'         processing the data.
#'
#' @examples
#' \dontrun{
#'   result <- handle_oddformats(data_frame, "file_path", "header_name", updated_crosswalk_data, existing_crosswalk_data, additional_info)
#'   }
#'
handle_oddformats <- function(data, fp, header, updatedCrosswalks, existingCrosswalks, info) {
  # Call the manual_update function to handle odd formats
  tmp <- manual_update(data, fp, header, updatedCrosswalks, existingCrosswalks, "")
  
  # Return the processed result
  return(tmp)
}

#' Manual Update for Crosswalk Data
#'
#' This function facilitates a manual update process for crosswalk data based on 
#' hardcoded parameters. It checks if a header needs to be updated manually and 
#' prompts the user to enter a value if it is not found in existing records.
#'
#' @param data A data frame containing the data to be updated.
#' @param fp A file path or identifier related to the data being processed.
#' @param header A character string specifying the header to be updated.
#' @param updatedCrosswalks A list of updated crosswalk mappings.
#' @param existingCrosswalks A file path to existing crosswalk mappings.
#' @param inputoptions Suggested options for user input if manual entry is required.
#'
#' @return A data frame with the updated header values replaced or an error message 
#'         prompting manual input.
#'
#' @examples
#' \dontrun{
#'   result <- manual_update(data_frame, "file_path", "header_name", updated_crosswalk_data, existing_crosswalk_data, suggested_options)
#'   }
#'
manual_update <- function(data, fp, header, updatedCrosswalks, existingCrosswalks, inputoptions) {
  
  # Take the hardcoded parameters that are already in the existing crosswalk
  hardparams <- updatedCrosswalks$HardcodedManualAttributes
  if(header %in% (hardparams$Header[hardparams$file == fp])) {
    # if the header is already entered for the file in question, select the value entered 
    # `|>` is the baseR version of the dplyr pipe `%>%`. They mostly work the same, but have slight differences. I'll note those differences in the function `reformat_data`.
    # I tried to change as many instances of `%>%` to `|>` as I could to reduce the number of times a specific package is needed
    found_param_manual <- hardparams |> dplyr::filter(Header == header, file == fp) |> dplyr::pull(Value)
  } else {
    # if the header isn't entered manual entry is needed
    # Generate the error message
    message = paste("Enter suspected", header, "value based on", fp, ". Suggested options are", paste(inputoptions, collapse = ", "))
    # Add the line to the hardcoded parameters data frame that needs to be filled out by the user
    hardparams_update <- hardparams |> add_row(file = fp, Header = header, Value = '')
    
    # Write the dataframe back to disk. The user can update it here. It will be tracked as a change by the targets package in the line `tar_target(existingCrosswalks, "DataCrosswalks", format = "file")` next time that tar_make() is run.
    write.csv(hardparams_update, file = file.path(existingCrosswalks, "HardcodedManualAttributes.csv"), row.names = FALSE)
    # Stopping the code here generates the message telling the user what to do, and also will send targets back to the existingCrosswalks target next time tar_make() is run.
    stop(message)
  }
  # Once the hardcoded parameters crosswalk is updated, the entered value will be stored as `found_param_manual`
  # The column designated by the `header` string in this function will be assigned the value of `found_param_manual` in the data frame
  # The following is written in dplyr-ese. 
  # `|>` is the native pipe operator
  # `!!` indicates to dplyr that it needs to find the variable outside of the dataframe `data`. 
  # Without including `!!`, dplyr::mutate will complain that there is no column called "header" in data. 
  # With `!!`, dplyr::mutate knows that it needs to use the value of header to look for a column in data.
  # `:=` is used here instead of `=` because header is a variable. Again, without it, dplyr::mutate would add a new column called "header" with the values of found_param_manual
  # With `:=`, the name of the column is the value of header
  # This could be done in base R pretty easily as well, but it would require multiple lines and my personal preference is to use dplyr because the pipes execute everything together.
  tmp <- data |> dplyr::mutate(!!header := found_param_manual)
  
  return(tmp)
}



#' Crosswalk Codes for Data Transformation
#'
#' This function applies a crosswalk transformation to specified codes in a given 
#' header of a data frame. It checks for new codes, updates them based on a crosswalk 
#' mapping, and generates the necessary mutate expressions.
#'
#' @param data A data frame containing the data to be transformed.
#' @param fp A file path or identifier related to the data being processed.
#' @param header A character string specifying the header to be transformed.
#' @param codescrosswalk A data frame containing the code crosswalk mappings.
#' @param forceupdate A logical indicating whether to enforce the presence of all codes 
#'                    in the crosswalk (default is TRUE).
#'
#' @return A data frame with the transformed column based on the specified crosswalk.
#'
#' @examples
#' \dontrun{
#'   result <- crosswalk_codes(data_frame, "file_path", "header_name", codes_crosswalk_data)
#'   }
#'
crosswalk_codes <- function(data, fp, header, codescrosswalk, forceupdate = TRUE) {
  header_tmp <- header
  # Filter the crosswalk based on the specified header
  codecrosswalk <- codescrosswalk |> 
    dplyr::filter(header == header_tmp)
  if(forceupdate) {
    # Check for any new codes not present in the crosswalk
    if(any(!unique(data[[header]]) %in% codecrosswalk$original_value)) {
      stop(paste("New codes", 
                 paste(unique(data[[header]])[!unique(data[[header]]) %in% codecrosswalk$original_value], collapse = ", "), 
                 "detected for", header, "in", fp))
    }
  }
  
  # Create the crosswalk mapping
  crosswalk <- codecrosswalk |> dplyr::select(original_value, new_value) |>
    dplyr::mutate(new_value = dplyr::case_when(grepl("NA", new_value) ~ new_value,
                                               .default = paste0('"', new_value, '"'))) |>
    dplyr::summarize(
      original_value = paste0('c("', paste(original_value, collapse = '", "'), '")'),
      .by = new_value) |>
    dplyr::mutate(expr = paste0(original_value, " ~ ", new_value))
  
  # Generate the mutate expression
  mutatecode <- ifelse(length(crosswalk$expr) > 0,
                       paste0(
                         "dplyr::mutate(., ", header_tmp, " = ", "dplyr::case_match(", header_tmp, ", ", paste(crosswalk$expr, collapse = ", "), ", .default = ", header_tmp, "))"
                       ),
                       ".")
  
  # Evaluate the mutate expression and return the updated data frame
  tmp <- data %>% {eval(parse(text = mutatecode))}
  
  return(tmp)
}


#' Standard Data Codes Treatment
#'
#' This function standardizes the treatment of data codes in a specified column of a 
#' data frame. It handles different scenarios based on the content and structure of the 
#' provided data. The function integrates various handling mechanisms for different case 
#' requirements.
#'
#' @param data A data frame containing the data to be treated.
#' @param filename A string representing the name of the file associated with the data.
#' @param header A character string specifying the header to be standardized.
#' @param updatedCrosswalks A list of updated crosswalk mappings.
#' @param existingCrosswalks A file path for existing crosswalk mappings.
#' @param force A logical vector indicating whether to forcibly update the codes (default is c(TRUE, FALSE)).
#'
#' @return A data frame with the standardized and treated data based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_datacodestreatment(data_frame, "filename.csv", "header_name", updated_crosswalk_data, existing_crosswalk_data)
#'   }
#'
standard_datacodestreatment <- function(data, filename, header, updatedCrosswalks, existingCrosswalks, force = c(TRUE, FALSE)) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      # Single column case
      if(detect_readme(filename, updatedCrosswalks)) {
        tmp <- handle_readmes(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else if(!is.character(data[[header]]) | 
                all(varhandle::check.numeric(gsub("E", "e", data[[header]])))) {
        tmp <- handle_headers(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else if(all(is.na(data[[header]]))) {
        tmp <- handle_headers(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else if(is.character(data[[header]])) {
        tmp <- crosswalk_codes(data = data, fp = filename, header = header, 
                               codescrosswalk = updatedCrosswalks$DataCodesCrosswalk, 
                               forceupdate = force)
      }
    } else if(length(grep(header, names(data))) > 1) {
      # Multiple columns case
      tmp <- concat_columns(data, header) |> 
        crosswalk_codes(fp = filename, header = header, codescrosswalk = updatedCrosswalks$DataCodesCrosswalk, forceupdate = force)
    }
    
  } else {tmp <- data} # If the header does not exist, return the data as is
  return(tmp)
}

#' Standard Name Treatment
#'
#' This function standardizes the treatment of names in a specified header of a 
#' data frame. It applies cleaning operations to the content of the specified name 
#' header, using a predefined list of common words to enhance data quality.
#'
#' @param data A data frame containing the data to be treated.
#' @param filename A string representing the name of the file associated with the data.
#' @param header A character string specifying the header to be standardized.
#' @param updatedCrosswalks A list of updated crosswalk mappings
#' @param existingCrosswalks A file path to existing crosswalk mappings 
#'
#' @return A data frame with the standardized names based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_nametreatment(data_frame, "filename.csv", "name_header", updated_crosswalk_data, existing_crosswalk_data)
#'   }
#'
standard_nametreatment <- function(data, filename, header, updatedCrosswalks, existingCrosswalks) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    # Single column case
    if(sum(names(data) == header) == 1) {
      if(all(is.na(data[[header]]))) {
        tmp <- handle_headers(data, filename, header, updatedCrosswalks, existingCrosswalks)
      } else {
        tmp <- data %>%
          dplyr::mutate(!!header := fedmatch::clean_strings(as.character(.[[header]]), common_words = fedmatch::corporate_words))
      }
    } else if(length(grep(header, names(data))) > 1) {
      # Multiple columns case
      tmp <- concat_columns(data, header) %>% 
        dplyr::mutate(!!header := fedmatch::clean_strings(as.character(.[[header]]), common_words = fedmatch::corporate_words))
    }
    
  } else {tmp <- data}  # If the header does not exist, return the data as is
  return(tmp)
}

#' Standard ID Treatment
#'
#' This function standardizes the treatment of identifiers in a specified header of a 
#' data frame. It ensures that the identifiers are treated as character types and handles 
#' cases for specific identifiers like "FacilityNumber".
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized identifiers based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_idtreatment(data_frame, "FacilityNumber")
#'   }
#'
standard_idtreatment <- function(data, header) {
  # Check if the header exists in the data
  if(sum(names(data) == header) == 1) {
    tmp <- data %>%
      dplyr::mutate(!!header := as.character(.[[header]]))
  } else if(length(grep(paste0(header, ".."), names(data))) > 1) {
    # Handle multiple variants of the specified header
    tmp <- concat_columns(data, header) %>% 
      dplyr::mutate(!!header := as.character(.[[header]]))
  } else {tmp <- data} # If the header does not exist, return the data as is
  # Specific handling for FacilityNumber
  if(header == "FacilityNumber" & sum(names(data) == "FacilityNumber") == 1 & 
     !"FacilityName" %in% names(tmp)) {
    tmp <- tmp %>% dplyr::filter(!is.na(FacilityNumber))
  }
  return(tmp)
}

#' Standard HUC Treatment
#'
#' This function standardizes the treatment of hydrological unit codes (HUC) in a 
#' specified header of a data frame. It trims the values in the specified header based 
#' on the parsed number from the header itself.
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized HUC values based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_HUCtreatment(data_frame, "HUCHeader")
#'   }
#'
standard_HUCtreatment <- function(data, header) {
  # Check if the header exists in the data
  if(header %in% names(data)) {
    n <- readr::parse_number(header)
    tmp <- data |> dplyr::mutate(!!header := substr(as.character(data[[header]]), 1, n))
  } else {tmp <- data} # If the header does not exist, return the data as is
  return(tmp)
}

#' Standard Address Treatment
#'
#' This function standardizes the treatment of address-related fields in a specified 
#' header of a data frame. It extracts and cleans components such as the address, city, 
#' state, and zip code from the specified header.
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized address components based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_Addresstreatment(data_frame, "AddressHeader")
#'   }
#'
standard_Addresstreatment <- function(data, header) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    entrylines <- concat_columns(data[c(grep(header, names(data)))], header) |> 
      pull(header) |>
      stringr::str_split(",") |> purrr::map(~stringr::str_trim(.x))
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
      
      if(type == "Address") {tmp <- str_to_title(address)} else if(
        type == "City") {tmp <- str_to_title(city)} else if(
          type == "State") {tmp <- state} else if(
            type == "Zip") {tmp <- zip}
      if(length(tmp) == 0) {tmp <- NA_character_}
      if(length(tmp) > 1) {tmp <- paste(tmp, collapse = ", ")}
      tmp
    }
    )
    
    tmp <- data |>
      dplyr::select(-contains(header)) |>
      dplyr::mutate(!!header := tmpvals)
    
  } else {tmp <- data} # If the header does not exist, don't do anything to the data
  tmp
}

#' Standard Coordinate Treatment
#'
#' This function standardizes the treatment of coordinate-related fields in a specified 
#' header of a data frame. It processes latitude and longitude values based on the header.
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized coordinate values based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_coordinatetreatment(data_frame, "CoordinateHeader")
#'   }
#'
standard_coordinatetreatment <- function(data, header) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      tmp <- handle_coordinates(data, header)
    } else if(length(grep(header, names(data))) > 1) {
      tmp1 <- handle_coordinates(data, header) |>
        concat_columns(header)  
      tmp <- tmp1 |> dplyr::mutate(!!header := gsub(",.*", "", tmp1[[header]]))
      
    }
  } else {tmp <- data}# If the header does not exist, return the data as is
  return(tmp)
}

#' Standard Year Treatment
#'
#' This function standardizes the treatment of year-related fields in a specified 
#' header of a data frame. It converts year values to numeric format and filters out 
#' rows with NA in the specified header.
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized year values based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_Yeartreatment(data_frame, "YearHeader")
#'   }
#'
standard_Yeartreatment <- function(data, header) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    if(length(grep(header, names(data))) == 1) {
      tmp <- data |> 
        dplyr::mutate(!!header := as.numeric(readr::parse_number(as.character(data[[header]])))) |>
        dplyr::filter(if_any(.cols = any_of(header), ~!is.na(.)))
    } else if(length(grep(header, names(data))) > 1) {
      stop("Is there more than one Year column?")
    }
  } else {tmp <- data}# If the header does not exist, return the data as is
  return(tmp)
}

# Definition of various representations of "NA" (Not Available) found in datasets
data_NAcodes <- c("", "n/a", "N/A", "NA", "NAN", "na", "nan", 
                  "not reported yet", "closed", NA)

#' Standard Data Treatment
#'
#' This function standardizes the treatment of specified data fields in a data frame. 
#' It checks for non-numeric values and converts valid entries to numeric, while handling 
#' specified NA representations.
#'
#' @param data A data frame containing the data to be treated.
#' @param header A character string specifying the header to be standardized.
#'
#' @return A data frame with the standardized data values based on the specified header.
#'
#' @examples
#' \dontrun{
#'   result <- standard_datatreatment(data_frame, "DataHeader")
#'   }
#'
standard_datatreatment <- function(data, header) {
  # Check if the header exists in the data
  if(length(grep(header, names(data))) > 0) {
    if(!is.numeric(data[[header]])) { 
      if(!all(unique(gsub("[[:digit:]]*|.", "", data[[header]])) %in% data_NAcodes)) {
        stop("New non-numeric data value detected")
      } else {
        suppressWarnings({
          tmp <- data |>
            dplyr::mutate(!!header := as.numeric(data[[header]]))
        })
      }
    } else {tmp <- data}
  } else {tmp <- data}
  return(tmp)
}

#' Reformat Data
#'
#' This function reformats a dataset by applying various standardization treatments 
#' to specified columns. It utilizes a list of treatment functions to ensure data 
#' consistency across different fields such as codes, names, IDs, HUCs, addresses, 
#' coordinates, years, and data values.
#'
#' @param x A data frame to be reformatted.
#' @param updatedCrosswalks A list containing updated crosswalk information.
#' @param existingCrosswalks A data path to existing crosswalk information.
#' @param data A character vector indicating whether to treat the data as "State" or "National".
#'
#' @return A reformatted data frame with standardized column values.
#'
#' @examples
#' \dontrun{
#'   result <- reformat_data(data_frame, updated_crosswalks, existing_crosswalks, "State")
#'   }
#'
reformat_data <- function(x, updatedCrosswalks, existingCrosswalks, data = c("State", "National")) {
  
  list(standard_datacodestreatment, standard_nametreatment, standard_idtreatment, 
       standard_HUCtreatment, standard_Addresstreatment, standard_coordinatetreatment,
       standard_Yeartreatment, standard_datatreatment) # call these to let the targets package know that they are used in this function
  
  # Define column groups for standardization
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
  
  # Create treatment code strings for each column type
  datacodes_f_code <- paste0(
    "purrr::imap(., ~standard_datacodestreatment(.x, .y, '", datacodecolumns_force, 
    "', updatedCrosswalks, existingCrosswalks, force = TRUE))", collapse = " %>% "
  )
  datacodes_u_code <- paste0(
    "purrr::imap(., ~standard_datacodestreatment(.x, .y, '", datacodecolumns_unforce, 
    "', updatedCrosswalks, existingCrosswalks, force = FALSE))", collapse = " %>% "
  )
  names_code <- paste0(
    "purrr::imap(., ~standard_nametreatment(.x, .y, '", namecolumns, 
    "', updatedCrosswalks, existingCrosswalks))", collapse = " %>% ")
  ids_code <- paste0("purrr::map(., ~standard_idtreatment(.x, '", idcolumns, "'))", collapse = " %>% ")
  HUCs_code <- paste0("purrr::map(., ~standard_HUCtreatment(.x, '", HUCcolumns, "'))", collapse = " %>% ")
  Addresses_code <- paste0("purrr::map(., ~standard_Addresstreatment(.x, '", Addresscolumns, "'))", collapse = " %>% ")
  coordinates_code <- paste0("purrr::map(., ~standard_coordinatetreatment(.x, '", coordinatecolumns, "'))", collapse = " %>% ")
  years_code <- paste0("purrr::map(., ~standard_Yeartreatment(.x, '", Yearcolumns, "'))", collapse = " %>% ")
  data_code <- paste0("purrr::map(., ~standard_datatreatment(.x, '", datacolumns, "'))", collapse = " %>% ")
  # As noted in `manual_updates`, `|>` is the base R pipe function
  # In the next block of code, I've used `|>` where possible, but I had to use `%>%` for the lines that parse the written code
  # The reason, I think, is that the `%>%` operator from dplyr allows you to pass an object (here a list) along using the `.` syntax while the `|>` operator does not.
  # Because the expressions being parsed are a little wonky, and because I used the `.` notation to pass along the object in the code, I had to use `%>%` here and `|>` didn't work.
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
  if(data == "State") {
    x_bystate <- purrr::map(state.abb, ~{st <- .x; purrr::keep_at(x_merge_ready, ~grepl(paste0("/", st, "/"), .))}); names(x_bystate) <- state.abb
    x_readystates <- purrr::keep(x_bystate, ~length(.) > 0)
    x_simplestates <- purrr::map(x_readystates, ~{purrr::reduce(.x, merge_andreplaceNA, .dir = "forward")})
    x_all <- do.call("bind_rows", x_simplestates) %>% 
      plugFacilityName(drop = FALSE)
  } else if(data == "National") {
    if(length(x_merge_ready) > 1) {
      x_all <- purrr::map(x_merge_ready, ~plugFacilityName(.x, drop = TRUE)) %>%
        purrr::reduce(merge_andreplaceNA, .dir = "forward")
    } else {x_all <- pluck(x_merge_ready, 1)}
    
  }

  
  ordered <- names(updatedCrosswalks$HeaderCrosswalk)
  
  x_ordered <- x_all |> dplyr::select(any_of(ordered))

  return(x_ordered)
}

#' Plug Facility Name
#'
#' This function updates the `FacilityName` in a data frame based on the values
#' in `FacilityNumber`. If `FacilityName` is missing (NA), it is replaced with the 
#' corresponding `FacilityNumber`. Optionally, rows with `FacilityName` = NA can be dropped.
#'
#' @param x A data frame that may contain `FacilityName` and `FacilityNumber`.
#' @param drop A logical value indicating whether to drop rows with NA `FacilityName`. 
#' Defaults to TRUE.
#'
#' @return A data frame with updated `FacilityName` and optionally dropped rows.
#'
#' @examples
#' \dontrun{
#'   updated_df <- plugFacilityName(data_frame, drop = FALSE)
#'   }
#'
plugFacilityName <- function(x, drop = TRUE) {
  if (all(c("FacilityName", "FacilityNumber") %in% names(x))) {
    tmp <- dplyr::mutate(x, FacilityName = dplyr::case_when(is.na(FacilityName) ~ FacilityNumber,
      !is.na(FacilityName) ~ FacilityName))
  } else {tmp <- x}
  
  if (drop == TRUE) {tmp <- filter(tmp, !is.na(FacilityName))}
  return(tmp)
}

#' Write All States Data
#'
#' This function takes a data frame and splits it by the `State` column, writing 
#' each group's data to a separate CSV file in a specified directory. Additionally, 
#' it saves the entire data frame as both a CSV and an R data file.
#'
#' @param x A data frame that should contain a `State` column.
#'
#' @return A message indicating the path to the saved CSV file for all states.
#'
#' @examples
#' \dontrun{
#'   write_allstates(data_frame)
#'   }
#'
write_allstates <- function(x) {
  purrr::map(dplyr::group_split(x, .by = State, .keep = FALSE), 
             ~{
               stname <- unique(.x$State)
               write.csv(.x, file.path("FormattedDataOutputs", "Statewise", paste0(stname, "_formatted.csv")), row.names = FALSE)
             })
  write.csv(x, "FormattedDataOutputs/AllStates.csv", row.names = FALSE)
  save(x, file = "FormattedDataOutputs/AllStates.RDa")
  return("FormattedDataOutputs/AllStates.csv")
}
