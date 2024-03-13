#' @title searchSTfiles
#' @description searches for str within all data files in state_data  (currently only functions 
#' for xlsx,xls,xlsm) \cr \cr
#' @param path_to_state_data character string path to state_data directory
#' @param fileTypes character vector of file types to search
#' @param maxSearchRow number indicating how many rows to search for string, if NA all rows will 
#' be searched
#' @param searchStr character string to search for (ignore.case=FALSE, will add option to change)
#' @return `allStrLines` data.frame with state, fileType, files, sheets, rowFound, colFound and 
#' cellContents
#' @examples 
#' ptm <- proc.time()
#'#set path to state data
#'path<-'./state_data/'
#'findSIC<-searchSTfiles(path,searchStr = "SIC")
#'proc.time() - ptm



searchSTfiles<-function(path_to_state_data,
                        fileTypes=c("xlsx","xls","xlsm","csv","dbf"),
                        maxSearchRow=5,
                        searchStr){
  library(readxl)
  #loop through state folders
  allStrLines<-data.frame(state=character(0),
                          fileType=character(0),
                          files=character(0),
                          sheets = character(0),
                          rowFound = numeric(0),
                          colFound = numeric(0),
                          cellContents = character(0))
  for (st in stFolders){
    #list all files recursively
    stfiles<-list.files(st,full.names = T,recursive = T)
    
    #subset files by fileTypes
    stfiles<-stfiles[tolower(tools::file_ext(basename(stfiles))) %in% fileTypes]
    
    if (length(stfiles)!=0){
      for (f in stfiles){
        #extract sheets from xlsx, xls, and xlsm files
        if (tolower(tools::file_ext(basename(f))) %in% c("xlsx","xls","xlsm") & 
            !startsWith(basename(f),"~")){
          fSheets<-readxl::excel_sheets(f)
          
          #loop through sheets
          for (sh in fSheets){
            
            if (!is.na(maxSearchRow)){
              searchFile<-suppressMessages(suppressWarnings(readxl::read_excel(f,sheet=sh,n_max=maxSearchRow+1,col_names = F) ))
            }else{
              searchFile<-suppressMessages(suppressWarnings(readxl::read_excel(f,sheet=sh,col_names = F) ))
            }
            searchFile<-as.data.frame(searchFile)
            if (length(searchFile)!=0){
              test<-t(searchFile)
              #test<-list(searchFile)
              test<-list(as.vector(unlist(list(searchFile))))
              test2<-data.frame(which(sapply(searchFile,`%in%`, test[[1]][grep(searchStr, test[[1]])]),arr.ind = T))
              if (nrow(test2)!=0){
                test2$value<-test[[1]][grep(searchStr,test[[1]])]
                names(test2)<-c("rowFound","colFound","cellContents")
                
                #compile list of files/sheets by state
                allStrs<-data.frame(state=rep(basename(dirname(f)),nrow(test2)),
                                    fileType=rep(tools::file_ext(basename(f)),nrow(test2)),
                                    files=rep(basename(f),nrow(test2)),
                                    sheets = rep(sh,nrow(test2))
                )
                allStrs<-cbind(allStrs,test2)
                allStrLines<-rbind(allStrLines,allStrs)
              }#if str found
            }#if sheet unempty
          }#each sheet
          
          
        }else{#not excel
          fSheets<-NA
          allStrLines<-allStrLines
        }
        
        
      }#each file
    }#if readable  files exist
  }#each state
  return(allStrLines)
}#end searchSTfiles

