#' @title findFormattedColumns
#' @description function to find columns in `testData` that meet the requirements
#' for the columns in the formatted state data found in columnTests.R\cr \cr
#' @param testData data.frame of test data
#' @param state character string indicating state abbreviation
#' @return data.frame with columns testColumn and formattedColname indicating 
#' that formatting test for the `formattedColname` passed
#' @import tigris
#' @import zipcodeR
#' @import purrr
#' @import data.table
#' @examples
#' testData<-structure(list(`From State Name` = c("Maryland", "Maryland", 
#'           "Maryland", "Maryland", "Maryland"), `From County Fips Code` = 
#'           c("047","047", "047", "045", "015"), `From County Name` = 
#'           c("Worcester County",  "Worcester County", "Worcester County", 
#'           "Wicomico County", "Cecil County")), row.names = c(NA, -5L), 
#'           class = c("tbl_df", "tbl", "data.frame"))
#' possibleMatchedCols<-findFormattedColumns(testData,"MD")

findFormattedColumns<-function(testData,state,fileName,sheetName){
  
  require(tigris)
  require(zipcodeR)
  require(purrr)
  require(data.table)
  
  formatCols<-c("SourceType", "HUC11", "BASIN", "SITE_NAME", "PERMIT_NUM", 
                "SourceID", "County", "HUC8", "YEAR", "Jan_mgd", "Feb_mgd", "Mar_mgd", 
                "Apr_mgd", "May_mgd", "Jun_mgd", "Jul_mgd", "Aug_mgd", "Sep_mgd", 
                "Oct_mgd", "Nov_mgd", "Dec_mgd", "Latitude", "Longitude", "SourceName", 
                "ANNUAL_WD_MGD", "NAICS.CODE", "STATE", "CATEGORY", "Town_SOURCE", 
                "REG_NUM", "Address_SOURCE", "FacilityID", "Address_OFFICE", 
                "Town_OFFICE", "CATEGORY2", "DESCRIPTION", "ALIAS", "SIC.CODE", 
                "Zip_SOURCE", "Saline", "HUC10", "HUC12")
  
  findCols<-lapply(formatCols, function(x) 
    lapply(names(testData), function(y) executeColumnTest(x,y,testData,state)))
  findCols<-lapply(purrr::transpose(findCols), function(x) do.call(rbind, x))
  findCols<-data.table::rbindlist(findCols)
  findCols<-findCols[which(findCols$testPassed),]
  findCols<-findCols[,c(1,2)]
  
  missingFormatCols<-formatCols[!formatCols %in% findCols$formattedColname]
  if (length(missingFormatCols)!=0){
  missingFormatCols<-data.frame(testColumn=rep(NA,length(missingFormatCols)),
                                formattedColname=missingFormatCols)
  findCols<-rbind(findCols,missingFormatCols)
  }
  
  findCols$fileName<-rep(fileName,nrow(findCols))
  findCols$sheetName<-rep(sheetName,nrow(findCols))
  return(findCols)
 
  
}


