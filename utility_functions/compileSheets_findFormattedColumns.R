#' @title compileSheets_findFormattedColumns
#' @description function to compile output from findFormattedColumns for multiple
#' sheets within `testFile` \cr \cr
#' @param testFile list object with multiple data.frames to test columns
#' @param state character string indicating state abbreviation
#' @return data.frame with columns testColumn and formattedColname indicating 
#' that formatting test for the `formattedColname` passed
#' @import tigris
#' @import zipcodeR
#' @import purrr
#' @import data.table
#' @examples
#' \dontrun{
#' compileSheets_findFormattedColumns(MDdat$`MD SWUDS IN data - Lovelace.xlsx`,state="MD")
#' }
#' 
compileSheets_findFormattedColumns<-function(testFile,state){

fileName<-strsplit(deparse(substitute(testFile)),"\\$")[[1]][2]
testData<-testFile
findCols<-as.data.frame(do.call(rbind,lapply(c(1:length(testData)),
                                             function(x) findFormattedColumns(testData[[x]],state,
                                                                              fileName = fileName,
                                                                              sheetName=names(testData)[x]))))
#sort by formattedColname
findCols<-findCols[order(findCols$formattedColname),]

return(findCols)
}