#' @title checkSTupdates
#' @description checks the NonSWUDS_Data_Input_Tracking.xlsx file for updates since last compilation\cr \cr
#' @param path_to_remote_tracker character string path to state_data directory
#' @param path_to_local_tracker
#' 
#' 
#' 
#' 
#' 


checkSTupdates <- function(path_to_remote_tracker, path_to_local_tracker) {
  
  exit <- function() {
    # https://stackoverflow.com/questions/17837289/break-exit-script
    .Internal(.invokeRestart(list(NULL, NULL), NULL))
  }
  continue<-TRUE
  # browser()
  require(readxl)
  
  remote_tracker <- as.data.frame(readxl::read_excel(path_to_remote_tracker))
  
  if(!file.exists(path_to_local_tracker)) {
    local_tracker <- remote_tracker
    write.csv(local_tracker, file = path_to_local_tracker, row.names = FALSE)
  }
  
  local_tracker <- as.data.frame(read.csv(path_to_local_tracker))
  
  if(!identical(remote_tracker, local_tracker)) {
    message("Warning: NonSWUDS data may have been updated since scripts were last run")
    
    message("Remote nonSWUDS tracker:")
    print(remote_tracker)
    message("Local nonSWUDS tracker:")
    print(local_tracker)

    msgText <- cat("Do you wish to proceed with the compilation? You will not receive this warning again.\n \n")
    continue <-menu(choices=c("Yes","No"),title=msgText)
    continue <- ifelse(continue == 1, TRUE, FALSE)
    if (!continue){
      return(list(continue = continue))
      exit()
    }
    
  } else if(identical(remote_tracker, local_tracker)) {message("No updates to NonSWUDS data since scripts were last run")}
  
  local_tracker <- remote_tracker
  local_tracker<-list(local_tracker = remote_tracker, continue=continue)
  return(local_tracker)
}
