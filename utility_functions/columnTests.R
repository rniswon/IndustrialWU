##################################
# Purpose: Store functions to test column names, class and values to match 
#          .\FormattedStateData\AllStates_formatted.csv
# Date: 2024-03-22
# Contact: lgormansanisaca@usgs.gov (Lily Gorman Sanisaca)
##################################

#' @title SourceType_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for SourceType \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"Source_Type"
#' testValues<-c("GW","SW","GW")
#' SourceType_test(testName,testValues,"AL")
SourceType_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("Source","src"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  #test values
  opts<-tolower(c("GW", "SW", "GROUND", "SURFACE", "Surface Water", "Groundwater", 
                  "TW", "GL","surfacewater","surface water"))
  value_test<-options_test(values,opts)
  
  testPass(c(value_test,name_test,class_test))
}


#' @title HUC11_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for HUC11 \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"HUC.11"
#' testValues<-c("01234567896","02345678912","03456789123")
#' HUC11_test(testName,testValues,"AL")
HUC11_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("HUC","hydrologic unit code",
                                    "hydrologic.unit.code",
                                    "hydrologic_unit_code"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer","character"))
  
  #test values
  value_test<-HUC_test(values,"HUC11")
  
  testPass(c(value_test,name_test,class_test))
}

#' @title BASIN_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for BASIN \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"basin id"
#' testValues<-c("basin1","basin2","basin3")
#' BASIN_test(testName,testValues,"AL")

BASIN_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("basin"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  testPass(c(name_test,class_test))
}

#' @title SITE_NAME_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for SITE_NAME \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"site name"
#' testValues<-c("thisSite","thatSite","site3")
#' SITE_NAME_test(testName,testValues,"AL")
#' 
#' testName<-"Site_ID"
#' testValues<-c(123,234,456)
#' SITE_NAME_test(testName,testValues,"AL")

SITE_NAME_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("site"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  testPass(c(name_test,class_test))
}


#' @title PERMIT_NUM_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for PERMIT_NUM \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"permitCode"
#' testValues<-c(123,"C456","456")
#' PERMIT_NUM_test(testName,testValues,"AL")

PERMIT_NUM_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("permit","npdes"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character",
                                      "integer","numeric"))
  
  testPass(c(name_test,class_test))
}

#' @title SourceID_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for SourceID \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"source.id"
#' testValues<-c(123,"SRC456","456")
#' SourceID_test(testName,testValues,"AL")

SourceID_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("source"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character",
                                      "integer","numeric"))
  
  testPass(c(name_test,class_test))
}

#' @title County_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for County \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @import tigris
#' @examples
#' testName<-"county name"
#' testValues<-c("Baltimore","Howard","Carol")
#' County_test(testName,testValues,"MD")

County_test<-function(name,values,state){
  require (tigris)
  #test name
  name_test<-nameTest(name,opts = c("county","cty",
                                    "co\\.",
                                    "cnty"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  #test values
  opts<-tolower(tigris::list_counties(state)$county)
  value_test<-options_test(values,opts)
  
  testPass(c(value_test,name_test,class_test))
}

#' @title HUC8_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for HUC8 \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"huc8"
#' testValues<-c(1234567,2345678,3456789)
#' HUC8_test(testName,testValues,"MD")

HUC8_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("HUC","hydrologic unit code",
                                    "hydrologic.unit.code",
                                    "hydrologic_unit_code"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer","character"))
  
  #test values
  value_test<-HUC_test(values,"HUC8")
  
  testPass(c(value_test,name_test,class_test))
}

#' @title YEAR_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for YEAR \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"year"
#' testValues<-c(2012,2013,2014)
#' YEAR_test(testName,testValues,"MD")

YEAR_test<-function(name, values, state){
  #test name
  name_test<-nameTest(name,opts = c("year"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer"))
  
  #test values
  maxDig<-max(sapply(values,nDigits),na.rm = T)
  minDig<-min(sapply(values,nDigits),na.rm = T)
  value_test<-ifelse(maxDig!=4 & minDig!=4,FALSE,TRUE)
  
  testPass(c(value_test,name_test,class_test))
}

#' @title Jan_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Jan_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"january"
#' testValues<-c(203,212,24)
#' Jan_mgd_test(testName,testValues,"MD")

Jan_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=1)
}


#' @title Feb_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Feb_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
Feb_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=2)
}

#' @title Mar_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Mar_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
Mar_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=3)
}

#' @title Apr_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Apr_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
Apr_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=4)
}

#' @title May_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for May_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
May_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=5)
}

#' @title Jun_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Jun_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Jun_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=6)
}

#' @title Jul_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Jul_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Jul_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=7)
}

#' @title Aug_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Aug_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Aug_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=8)
}

#' @title Sep_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Sep_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Sep_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=9)
}

#' @title Oct_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Oct_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Oct_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=10)
}

#' @title Nov_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Nov_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Nov_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=11)
}

#' @title Dec_mgd_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for Dec_mgd \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
Dec_mgd_test<-function(name,values,state){
  monthMGDtest(name,values,monthNumber=12)
}

#' @title Latitude_test
#' @description function to test if column names and class valid for Latitude \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
#' @examples
#' testName<-"lat"
#' testValues<-c(23.4567,23.7896)
#' Latitude_test(testName,testValues)
Latitude_test<-function(name, values, state){
  #test name
  name_test<-nameTest(name,opts = c("latitude","lat"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer"))
  
  testPass(c(name_test,class_test))
}


#' @title Longitude_test
#' @description function to test if column names and class valid for Longitude \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
#' @examples
#' testName<-"lat"
#' testValues<-c(23.4567,23.7896)
#' Longitude_test(testName,testValues)
Longitude_test<-function(name, values, state){
  #test name
  name_test<-nameTest(name,opts = c("longitude","long"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer"))
  
  testPass(c(name_test,class_test))
}


#' @title SourceName_test
#' @description function to test if column names and class are in set 
#' of valid options for SourceName \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"Source.Name"
#' testValues<-c("Coastal Plain","G213456","Some Water Company")
#' SourceName_test(testName,testValues,"AL")
SourceName_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("Source","src"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  #test values
  opts<-tolower(c("GW", "SW", "GROUND", "SURFACE", "Surface Water", "Groundwater", 
                  "TW", "GL","surfacewater","surface water"))
  value_test<-options_test(values,opts)
  #dont confuse with SourceType values
  value_test<-ifelse(value_test,FALSE,TRUE)
  
  testPass(c(value_test,name_test,class_test))
}

#' @title ANNUAL_WD_MGD_test
#' @description function to test if column names and class are in set 
#' of valid options for ANNUAL_WD_MGD \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"annual withdrawal"
#' testValues<-c(25.635,35.645)
#' ANNUAL_WD_MGD_test(testName,testValues,"AL")
ANNUAL_WD_MGD_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("annual","yearly"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric"))
  
  testPass(c(name_test,class_test))
}


#' @title NAICS.CODE_test
#' @description function to test if column names and class are in set 
#' of valid options for NAICS.CODE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"naics code"
#' testValues<-as.integer(c(334513,423830))
#' NAICS.CODE_test(testName,testValues,"AL")
NAICS.CODE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("naic"))
  
  #test class
  class_test<-classTest(values,opts=c("integer","character"))
  
  #test values
  opts<-get_naic_codes()
  value_test<-options_test(values,opts)
  
  testPass(c(name_test,class_test,value_test))
}


#' @title STATE_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for STATE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @import tigris
#' @examples
#' testName<-"state.abb"
#' testValues<-c("MD","AL","NY")
#' STATE_test(testName,testValues,"MD")

STATE_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("state"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  #test values
  opts<-tolower(c(state.abb,state.name))
  value_test<-options_test(values,opts)
  
  testPass(c(value_test,name_test,class_test))
}


#' @title CATEGORY_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for CATEGORY \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @import tigris
#' @examples
#' testName<-"site.category"
#' testValues<-c("TE","IND","NY")
#' CATEGORY_test(testName,testValues,"MD")

CATEGORY_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("category"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  #test values
  opts<-tolower(c("ind","co","com","ir","TE"))
  value_test<-options_test(values,opts)
  
  testPass(c(value_test,name_test,class_test))
}


#' @title Town_SOURCE_test
#' @description function to test if column names and class are in set 
#' of valid options for Town_SOURCE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"source.town"
#' testValues<-c("frederick","baltimore")
#' Town_SOURCE_test(testName,testValues,"MD")
Town_SOURCE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("town","city"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  #test values
  opts<-tolower(c("GW", "SW", "GROUND", "SURFACE", "Surface Water", "Groundwater", 
                  "TW", "GL","surfacewater","surface water"))
  value_test<-options_test(values,opts)
  #dont confuse with SourceType values
  value_test<-ifelse(value_test,FALSE,TRUE)
  
  testPass(c(value_test,name_test,class_test))
}


#' @title REG_NUM_test
#' @description function to test if column names and class are in set 
#' of valid options for REG_NUM \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"region number"
#' testValues<-as.integer(c(21315802,31803001))
#' REG_NUM_test(testName,testValues,"AL")
REG_NUM_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("region","reg"))
  
  #test class
  class_test<-classTest(values,opts=c("integer","character","factor"))
  
  testPass(c(name_test,class_test))
}


#' @title Address_SOURCE_test
#' @description function to test if column names and class are in set 
#' of valid options for Address_SOURCE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"address.src"
#' testValues<-c("116 WORCHESTER ST","771 DONALD LYNCH BLVD.")
#' Address_SOURCE_test(testName,testValues,"AL")
Address_SOURCE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("address","add"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  testPass(c(name_test,class_test))
}



#' @title FacilityID_test
#' @description function to test if column names and class are in set 
#' of valid options for FacilityID \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"facility.id.code"
#' testValues<-as.integer(c(265,251))
#' FacilityID_test(testName,testValues,"AL")
FacilityID_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("facility"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor","integer"))
  
  testPass(c(name_test,class_test))
}


#' @title Address_OFFICE_test
#' @description function to test if column names and class are in set 
#' of valid options for Address_OFFICE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"office add"
#' testValues<-c("116 WORCHESTER ST","771 DONALD LYNCH BLVD.")
#' Address_OFFICE_test(testName,testValues,"AL")
Address_OFFICE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("address","add","office"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  testPass(c(name_test,class_test))
}

#' @title Town_OFFICE_test
#' @description function to test if column names and class are in set 
#' of valid options for Town_OFFICE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"office.city"
#' testValues<-c("frederick","baltimore")
#' Town_OFFICE_test(testName,testValues,"MD")
Town_OFFICE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("town","city","office"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  testPass(c(name_test,class_test))
}


#' @title CATEGORY2_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for CATEGORY2 \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @import tigris
#' @examples
#' testName<-"subsite.category"
#' testValues<-c("TE23","IND09","NY2")
#' CATEGORY2_test(testName,testValues,"MD")

CATEGORY2_test<-function(name,values,state){
  #test name
  name_test<-nameTest(name,opts = c("category"))
  
  #test class
  class_test<-classTest(values,opts=c("factor","character"))
  
  #test values
  opts<-tolower(c("ind","co","com","ir","TE"))
  value_test<-options_test(values,opts)
  #dont use CATEGORY values
  value_test<-ifelse(value_test,FALSE,TRUE)
  
  testPass(c(value_test,name_test,class_test))
}

#' @title DESCRIPTION_test
#' @description function to test if column names and class are in set 
#' of valid options for DESCRIPTION \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"desc"
#' testValues<-c("Agricultural/Food Processing","Sand and Gravel Washing")
#' DESCRIPTION_test(testName,testValues,"MD")
DESCRIPTION_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("description","desc"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  testPass(c(name_test,class_test))
}

#' @title ALIAS_test
#' @description function to test if column names and class are in set 
#' of valid options for ALIAS \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"desc"
#' testValues<-c("Strata Corporation","Cold Spring Brewery Company")
#' ALIAS_test(testName,testValues,"MD")
ALIAS_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("alias"))
  
  #test class
  class_test<-classTest(values,opts=c("character","factor"))
  
  testPass(c(name_test,class_test))
}

#' @title SIC.CODE_test
#' @description function to test if column names and class are in set 
#' of valid options for SIC.CODE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"sic_code"
#' testValues<-as.integer(c(1200,2900))
#' SIC.CODE_test(testName,testValues,"AL")
SIC.CODE_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("sic"))
  
  #test class
  class_test<-classTest(values,opts=c("integer","character"))
  
  #test values
  opts<-get_sic_codes()
  value_test<-options_test(values,opts)
  
  testPass(c(name_test,class_test,value_test))
}


#' @title Zip_SOURCE_test
#' @description function to test if column names, class, values are in set 
#' of valid options for Zip_SOURCE \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE
#' @import zipcodeR 
#' @examples
#' testName<-"zip"
#' testValues<-as.integer(c(21227,21228))
#' Zip_SOURCE_test(testName,testValues,"MD")
Zip_SOURCE_test<-function(name,values,state){
  require(zipcodeR)
  #test name
  name_test<-nameTest(name,opts = c("zip","zipcode"))
  
  #test class
  class_test<-classTest(values,opts=c("integer"))
  
  #test values
  opts<-as.integer(zipcodeR::search_state(state)$zipcode)
  value_test<-options_test(values,opts)
  
  testPass(c(value_test,name_test,class_test))
}


#' @title Saline_test
#' @description function to test if column names and class are in set 
#' of valid options for Saline \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"saline"
#' testValues<-c(TRUE, TRUE , FALSE)
#' Saline_test(testName,testValues,"AL")
Saline_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("saline"))
  
  #test class
  class_test<-classTest(values,opts=c("logical"))
  
  testPass(c(name_test,class_test))
}

#' @title HUC10_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for HUC10 \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"HUC.10"
#' testValues<-c("0123456789","0234567891","0345678912")
#' HUC10_test(testName,testValues,"AL")
HUC10_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("HUC","hydrologic unit code",
                                    "hydrologic.unit.code",
                                    "hydrologic_unit_code"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer","character"))
  
  #test values
  value_test<-HUC_test(values,"HUC10")
  
  testPass(c(value_test,name_test,class_test))
}




#' @title HUC12_test
#' @description function to test if column names, class, and values are in set 
#' of valid options for HUC12 \cr \cr
#' @param name character string of column name
#' @param values vector of values to test
#' @param state character string indicating state abbreviation
#' @return TRUE/FALSE 
#' @examples
#' testName<-"HUC.12"
#' testValues<-c("012345678912","023456789112","034567891212")
#' HUC12_test(testName,testValues,"AL")
HUC12_test<-function(name,values,state){
  
  #test name
  name_test<-nameTest(name,opts = c("HUC","hydrologic unit code",
                                    "hydrologic.unit.code",
                                    "hydrologic_unit_code"))
  
  #test class
  class_test<-classTest(values,opts=c("numeric","integer","character"))
  
  #test values
  value_test<-HUC_test(values,"HUC12")
  
  testPass(c(value_test,name_test,class_test))
}


#' @title executeColumnTest
#' @description function to execute individual column test on single column of 
#' test data\cr \cr
#' @param formatName character string of column name in formatted state data, 
#' used to identify the correct column test
#' @param testColName character string of column name in test data
#' @param testData data.frame of test data
#' @param state character string indicating state abbreviation
#' @return data.frame with columns testColumn formattedColname, and testPassed 
#' indicating whether the column indicated by `testColName` passes formatting 
#' test indicated by `formatName`
#' @examples
#' formatName<-"STATE"
#' testName<-"From State Name"
#' testData<-structure(list(`From State Name` = c("Maryland", "Maryland", 
#'           "Maryland", "Maryland", "Maryland"), `From County Fips Code` = 
#'           c("047","047", "047", "045", "015"), `From County Name` = 
#'           c("Worcester County",  "Worcester County", "Worcester County", 
#'           "Wicomico County", "Cecil County")), row.names = c(NA, -5L), 
#'           class = c("tbl_df", "tbl", "data.frame"))
#' state<-"MD"
#' executeColumnTest(formatName,testName,testData,state)
#' executeColumnTest(formatName,testColName="From County Fips Code",testData,state)
executeColumnTest<-function(formatName,testColName,testData,state){
  strTest<-paste0(formatName,"_test('",testColName,"',",
                  deparse(substitute(testData)),"$`",testColName,"`,'",state,"')")
  pass<-eval(parse(text=strTest))
  pass<-data.frame(testColumn = testColName,
                   formattedColname = formatName, 
                   testPassed = pass)
  return(pass)
}