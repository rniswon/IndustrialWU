##################################
# Purpose: Store functions utility functions for functions in columnTests.R to 
#          test column names, class and values to match 
#          .\FormattedStateData\AllStates_formatted.csv
# Date: 2024-03-22
# Contact: lgormansanisaca@usgs.gov (Lily Gorman Sanisaca)
##################################


#function to test if values are in set of valid options

#' @title options_test
#' @description function to test if values are in set of valid options \cr \cr
#' @param values vector of values to test
#' @param opts vector of accepted values
#' @param allMatch TRUE/FALSE indicating if all `values` should be found in 
#' `opts`
#' @return TRUE/FALSE 
#' @examples 
#'testVals<-c(state.abb,state.name)
#'opts<-c("AL","Alabama")
#'options_test(testVals,opts)

options_test<-function(values,opts, allMatch=FALSE){
  opts<-tolower(opts)
  uniqueVals<-tolower(unique(na.omit(values)))
  inOpts<-uniqueVals %in% opts
  test<-ifelse(allMatch,all(inOpts),any(inOpts))
  if (test){
    pass<-TRUE
  }else{
    pass<-FALSE
  }
  return(pass)
}



#' @title nDigits
#' @description function to count digits \cr \cr
#' @param x value to count, can be character or number class
#' @return number of digits or characters 
#' @examples 
#' nDigits("012345567")
#' nDigits(12345678)

nDigits <- function(x){
   nchar(as.character(x))
}


#' @title findLeadZero
#' @description function to identify leading zeros \cr \cr
#' @param values vector of characters or numbers
#' @return TRUE/FALSE indicating if leading zeros are present
#' @examples 
#' testVals<-c("0123","0234","0345")
#' testVals2<-c(123,234,345)
#' findLeadZero(testVals)
#' findLeadZero(testVals2)

findLeadZero<- function(values) {
  leadZero<-unique(sapply(values,function(x)
    ifelse(substr(as.character(x),1,1)=="0",TRUE,FALSE)))
  leadZero<-ifelse(length(leadZero)==1,leadZero,FALSE)
  leadZero<-ifelse(!leadZero %in% c(TRUE,FALSE),FALSE,leadZero)
  return(leadZero)
}


#' @title HUC_test
#' @description function to test length of possible HUC data \cr \cr
#' @param values vector of characters or numbers
#' @param huc_level character string in form HUC11, HUC8, etc. or number 
#' @return TRUE/FALSE indicating if correct number of digits present for 
#' `huc_level`
#' @examples 
#' testHuc<-c("01234567","02345678")
#' testHucNoLeadZero<-c(1234567,2345678)
#' HUC_test(testHuc,"HUC11")
#' HUC_test(testHuc,"HUC8")
#' HUC_test(testHucNoLeadZero,"HUC8")

HUC_test<-function(values,huc_level){
  maxDig<-max(sapply(values,nDigits),na.rm = T)
  leadZero<-findLeadZero(values)
  if (any(class(values)=="character") | leadZero==TRUE){
    testLength<-as.numeric(gsub("HUC","",huc_level))
  }else{
    testLength<-as.numeric(gsub("HUC","",huc_level))-1
  }
  
  if (maxDig==testLength){
    pass<-TRUE
  }else{
    pass<-FALSE
  }
  
  return(pass)
}


#' @title nameTest
#' @description test if colname in vector of options \cr \cr
#' @param name character string of column name
#' @param opts vector of accepted values
#' @return TRUE/FALSE indicating `opts` are somewhere in `name`
#' @examples 
#' testName<-"SITE_NAME"
#' opts<-c("site","siteID")
#' nameTest(testName,opts)

nameTest<-function(name,opts){
  inOpts<-sapply(tolower(opts),function(x) ifelse(regexpr(x,tolower(name))>0,TRUE,FALSE))
  if (any(inOpts)){
    pass<-TRUE
  }else{
    pass<-FALSE
  }
  return(pass)
}

#' @title classTest
#' @description test if values in class \cr \cr
#' @param values vector of values to test class
#' @param opts vector of accepted classes
#' @return TRUE/FALSE indicating class is accepted
#' @examples 
#' testClass<-c(1,2,3,"L")
#' opts<-c("numeric","integer")
#' classTest(testClass,opts)
#' classTest(c(1,2,3),opts)

classTest<-function(values,opts){
  classVal<-class(values)
  pass<-options_test(classVal,opts)
  return(pass)
}



#' @title monthMGDtest
#' @description test for monthly mgd\cr \cr
#' @param name character string of column name
#' @param values vector of values
#' @param monthNumber number indicating which month is being tested
#' @return TRUE/FALSE indicating if possible monthly mgd data
#' @examples 
#' testName<-"Jan_mgd"
#' testVals<-c(23,44,67)
#' monthNumber<-1
#' monthMGDtest(testName,testVals,monthNumber)
#' monthMGDtest(testName,testVals,2)

monthMGDtest<-function(name,values,monthNumber){
  #test name
  name_test<-nameTest(name,opts = c(tolower(month.name[monthNumber]),
                                    tolower(month.abb[monthNumber])))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer"))
  
  testPass(c(name_test,class_test))
  
}



#' @title testPass
#' @description test for pass all tests\cr \cr
#' @param tests logical vector indicating a TRUE/FALSE for pass/fail of each 
#' test
#' @return TRUE/FALSE indicating whether all tests were passed
#' @examples 
#' value_test<-TRUE
#' class_test<-TRUE
#' name_test<-FALSE
#' testPass(c(value_test,class_test,name_test))
#' testPass(c(value_test,class_test,TRUE))

testPass<-function(tests){
  
  if (all(tests)){
    pass<-TRUE
  }else{
    pass<-FALSE
  }
  
  return(pass)
}