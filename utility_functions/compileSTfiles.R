#' @title compileSTfiles
#' @description compiles list of all files and sheets found in state_data \cr \cr
#' @param path_to_state_data character string path to state_data directory
#' @return `allSTfiles` data.frame with state, fileType, files, sheets
#' @examples 
#' ptm <- proc.time()
#'#set path to state data
#'path<-'./state_data/'
#'compiledSTfiles<-compileSTfiles(path)
#'proc.time() - ptm

compileSTfiles<-function(path_to_state_data){
  library(readxl)
  #find all state folders
  stFolders<-list.dirs(path,full.names = T)
  stFolders<-stFolders[basename(stFolders) %in% state.abb]
  
  #loop through state folders
  allSTfiles<-data.frame(state=character(0),fileType=character(0),files=character(0),sheets = character(0))
  for (st in stFolders){
    #list all files recursively
    stfiles<-list.files(st,full.names = T,recursive = T)
    
    for (f in stfiles){
      #extract sheets from xlsx, xls, and xlsm files
      if (tolower(tools::file_ext(basename(f))) %in% c("xlsx","xls","xlsm") & 
          !startsWith(basename(f),"~")){
        fSheets<-readxl::excel_sheets(f)  
      }else{
        fSheets<-NA
      }
      #compile list of files/sheets by state
      allSheets<-data.frame(state=rep(basename(dirname(f)),length(fSheets)),
                            fileType=rep(tools::file_ext(basename(f)),length(fSheets)),
                            files=rep(basename(f),length(fSheets)),
                            sheets = fSheets)
      allSTfiles<-rbind(allSTfiles,allSheets)
      
    }#each file
    
  }#each state
  return(allSTfiles)
}#end compileSTfiles function


