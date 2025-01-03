### program cleans addresses on SWUDS/nonSWUDS water use files
###   and matches to master v4 industrial facility list by
###   city+address

# Set working directory
setwd("C:/Home_Local/clluukko/Wateruse/IND_modeling")

# Load packages.
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)


######################################################################
### Read master industrial facility list and SWUDs/nonSWUDS datafiles

d_WU <- read.delim("SWUDSandNONSWUDS_AllStates_1028.csv", header = TRUE, sep = ",") 

d_IND <- read.delim("USEPA_HIFLD_EIA_PPP_facility_v4.csv", header = TRUE, sep = ",") 



## Need to change city and address to all caps
d_WU$City1 <- toupper(d_WU$City1)
d_WU$Address1 <- toupper(d_WU$Address1)
d_WU$FacilityName <- toupper(d_WU$FacilityName)
d_WU$County1 <- toupper(d_WU$County1)


#adds a dataset column = Duplicate address column   # Relocate ADDRESSE and ADRSFLAG
d_WU_Fix <- d_WU %>%
  mutate(ADDRESS_Fix = Address1) %>%
  relocate(ADDRESS_Fix, .after=Address1) %>%
  mutate(NAME_FIX = FacilityName) %>%
  relocate(NAME_FIX, .after = FacilityName)

d_IND_Fix <- d_IND %>%
  mutate(NAME_FIX = NAME) %>%
  relocate(NAME_FIX, .after = NAME)

d_IND_Fix$NAME_FIX <- toupper(d_IND_Fix$NAME_FIX)

######################
### Cleanup addresses

d_WU_Fix$ADDRESS_Fix <- gsub("\\bRDS\\b|\\bRD\\b|\\bRD\\.|\\bRD\\,", "ROAD",
                        gsub("\\bCTH\\b", "COUNTY HIGHWAY",
                        gsub("\\bST RTE\\b", "STATE ROUTE",
                        gsub("\\bLN\\b|\\bLN\\.|\\bLN\\,", "LANE",
                        gsub("\\bSTE\\b|\\bSTE\\.", "SUITE",
                        gsub("\\bST\\,|\\bST\\.|\\bSTREET\\b|\\bSTREET\\,|\\bST\\.\\,", "ST",
                        gsub("\\bRTE\\b", "ROUTE",
                        gsub("\\bHWY\\b|\\bHWY\\.|\\bHWY\\,", "HIGHWAY",
                        gsub("\\bAVE\\b|\\bAVE\\.|\\bAVE\\,|\\bAV\\b", "AVENUE",
                        gsub("\\bBYP$", "BYPASS", 
                        gsub("\\bPKWY", "PARKWAY", 
                        gsub("\\bCIR\\b", "CIRCLE",
                        gsub("\\bBLV\\b|\\bBLVD\\b|\\bBLVD\\.|\\bBLVD\\,", "BOULEVARD", 
                        gsub("\\bBLD\\b|\\bBLDG\\b", "BUILDING",
                        gsub("\\bDRIVE\\b|\\bDR\\.|\\bDR\\,|\\bDR\\.\\,", "DR", 
                        gsub("\\bEAST\\b|\\bE\\.", "E", 
                        gsub("\\bWEST\\b|\\bW\\.", "W",
                        gsub("\\bSOUTH\\b|\\bS\\.", "S",
                        gsub("\\bNORTH\\b|\\bN\\.|\\bNORTH\\,", "N",
                        gsub("\\bAPT ", "APARTMENT", 
                        gsub("\\bPL$|\\bPL\\.", "PLACE",
                        gsub("\\bPLZ\\b", "PLAZA",
                        gsub("\\bCT\\b|\\bCT\\.|\\bCT\\,", "COURT",
                        gsub("\\bCTR\\b", "CENTER",
                        gsub("\\bTRL$", "TRAIL", 
                        gsub("\\bTPKE\\b", "TURNPIKE", 
                        gsub("\\s$", "", d_WU_Fix$ADDRESS_Fix)))))))))))))))))))))))))))	

## CHECK FOR NON-ADDRESS CHARACTER AND ADDS A FLAG TO CHECK THESE ADDRESSES OR IF ADDRESS <> ADDRESS2
d_WU_Fix <- d_WU_Fix %>%
  mutate(ADRSFLAG = case_when(grepl("MILE", d_WU_Fix$Address1) ~ "CHECK",
                              grepl(" MI ", d_WU_Fix$Address1) ~ "CHECK", # added space
                              grepl("FEET", d_WU_Fix$Address1) ~ "CHECK",
                              grepl(" FT ", d_WU_Fix$Address1) ~ "CHECK", # added space
                              grepl("APPROX", d_WU_Fix$Address1) ~ "CHECK",
                              grepl("UNKNOWN", d_WU_Fix$Address1) ~ "CHECK",
                              grepl("[\\(]", d_WU_Fix$Address1) ~ "CHECK",
                              grepl("[:punct:&&[^-]]", d_WU_Fix$Address1)~ "CHECK", #checks for punctuation but not -
                              grepl(" BOX ", d_WU_Fix$Address1)~ "CHECK",
                              grepl("\\P O", d_WU_Fix$Address1)~ "CHECK",
                              !grepl("\\d", d_WU_Fix$Address1) ~ "CHECK",        #no digits
                              (d_WU_Fix$ADDRESS_Fix != d_WU_Fix$Address1) ~ "UPDATED",
                              .default = "OK"))%>%
  relocate(ADRSFLAG, .after=ADDRESS_Fix)


######################
### Cleanup names

d_WU_Fix$NAME_FIX<-gsub("\\bINC\\.\\b|\\bINC\\b|\\bINCOPORATED\\b","INCORPORATED",
                   gsub("\\bCORP\\b|\\bCORP\\.\\b","CORPORATION", 
                   gsub("\\bLLC\\b|\\bLLC\\.\\b|\\bL\\.L\\.C\\.","LIMITED LIABILITY COMPANY",
                   gsub("\\bLIMITED LIABILITY CORPORATION\\b", "LIMITED LIABILITY COMPANY",
                   gsub("\\bLTD\\b|\\bLTD\\.\\b|Limtd","LIMITED", 
                   gsub("\\bLP\\b|\\bLP\\b|\\bL\\.P\\.","LIMITED PARTNERSHIP",      
                   gsub("\\bDIV\\b|DIV\\.\\b","DIVISION",
                   gsub("\\bMFG\\b|\\bMFG\\.\\b","MANUFACTURING",
                   gsub("\\bCNTY\\b|\\bCNTY\\.\\b","COUNTY",
                   gsub("\\bCNTRS\\b|\\bCNTRS\\.\\b|\\bCNTR\\b","CENTERS",
                   gsub("\\bSYS\\b|\\bSYS\\.","SYSTEM",
                   gsub("\\bDEPT\\b|\\bDEPT\\.","DEPARTMENT", 
                   gsub("\\bINTL\\b", "INTERNATIONAL",
                   gsub("\\&","AND",
                   gsub("\\,","",
                   gsub("\\s+", " ", 
                   gsub("\\bCO\\b|\\bCO\\.\\b|\\bCOPANY\\b|\\bCO\\.\\,\\b", "COMPANY", d_WU_Fix$NAME_FIX)))))))))))))))))
#d_WU_Fix <- d_WU_Fix %>%relocate(NAME_Fix, .after=FacilityName)


write.table(d_WU_Fix, "WUfile_Fix_with_Flags.txt", sep="\t", row.names=FALSE)


######################
### Cleanup names in industrial facility lists

d_IND_Fix$NAME_FIX<-gsub("\\bINC\\.\\b|\\bINC\\b|\\bINCOPORATED\\b","INCORPORATED",
                    gsub("\\bCORP\\b|\\bCORP\\.\\b","CORPORATION", 
                    gsub("\\bLLC\\b|\\bLLC\\.\\b|\\bL\\.L\\.C\\.","LIMITED LIABILITY COMPANY",
                    gsub("\\bLIMITED LIABILITY CORPORATION\\b", "LIMITED LIABILITY COMPANY",
                    gsub("\\bLTD\\b|\\bLTD\\.\\b|Limtd","LIMITED", 
                    gsub("\\bLP\\b|\\bLP\\b|\\bL\\.P\\.","LIMITED PARTNERSHIP",      
                    gsub("\\bDIV\\b|DIV\\.\\b","DIVISION",
                    gsub("\\bMFG\\b|\\bMFG\\.\\b","MANUFACTURING",
                    gsub("\\bCNTY\\b|\\bCNTY\\.\\b","COUNTY",
                    gsub("\\bCNTRS\\b|\\bCNTRS\\.\\b|\\bCNTR\\b","CENTERS",
                    gsub("\\bSYS\\b|\\bSYS\\.","SYSTEM",
                    gsub("\\bDEPT\\b|\\bDEPT\\.","DEPARTMENT", 
                    gsub("\\bINTL\\b", "INTERNATIONAL",
                    gsub("\\&","AND",
                    gsub("\\,","",
                    gsub("\\s+", " ", 
                    gsub("\\bCO\\b|\\bCO\\.\\b|\\bCOPANY\\b|\\bCO\\.\\,\\b", "COMPANY", d_IND_Fix$NAME_FIX)))))))))))))))))
#d_IND <- d_IND %>%relocate(d_IND, .after=NAME)



## Create city+address_fix and county+facility name_fix fields

d_WU_Fix2 <- d_WU_Fix %>%
  mutate(CITY_ADDRESS = case_when(
    is.na(City1) ~ "",
    is.na(ADDRESS_Fix) ~ "",
    (!is.na(City1) & !is.na(ADDRESS_Fix)) ~ paste0(d_WU_Fix$City1, "_", 
                                                  d_WU_Fix$ADDRESS_Fix)
  )) %>%
  mutate(COUNTY_NAME = case_when(
    is.na(County1) ~ "",
    is.na(NAME_FIX) ~ "",
    (!is.na(County1) & !is.na(NAME_FIX)) ~ paste0(d_WU_Fix$County1, "_",
                                                    d_WU_Fix$NAME_FIX)
  )) %>%
  mutate(COUNTY_ADDRESS = case_when(
    is.na(County1) ~ "",
    is.na(ADDRESS_Fix) ~ "",
    (!is.na(County1) & !is.na(ADDRESS_Fix)) ~ paste0(d_WU_Fix$County1, "_",
                                                  d_WU_Fix$ADDRESS_Fix)
  )) %>%
  mutate(CITY_NAME = case_when(
    is.na(City1) ~ "",
    is.na(NAME_FIX) ~ "",
   (!is.na(City1) & !is.na(ADDRESS_Fix)) ~ paste0(d_WU_Fix$City1, "_",
                                                     d_WU_Fix$NAME_FIX)
  )) %>%
  mutate(STATE_NAME = case_when(
    is.na(State) ~ "",
    is.na(NAME_FIX) ~ "",
    (!is.na(State) & !is.na(NAME_FIX)) ~ paste0(d_WU_Fix$State, "_",
                                                   d_WU_Fix$NAME_FIX) 
  ))

#write.csv(d_WU_Fix2, "match_check.csv", row.names = FALSE)


d_IND_Fix$CITY_ADDRESS <- paste0(d_IND_Fix$CITY, "_", 
                            d_IND_Fix$ADDRESS_Fix)
d_IND_Fix$COUNTY_NAME <- paste0(d_IND_Fix$COUNTY, "_",
                           d_IND_Fix$NAME_FIX)
d_IND_Fix$COUNTY_ADDRESS <- paste0(d_IND_Fix$COUNTY, "_",
                                d_IND_Fix$ADDRESS_Fix)
d_IND_Fix$CITY_NAME <- paste0(d_IND_Fix$CITY, "_", 
                                 d_IND_Fix$NAME_FIX)
d_IND_Fix$STATE_NAME <- paste0(d_IND_Fix$STATE, "_",
                               d_IND_Fix$NAME_FIX)

#write.csv(d_IND_Fix, "match_check2.csv", row.names = FALSE)


#record_WU_IDX <- match(d_WU_Fix2$CITY_ADDRESS, 
#                        d_IND$CITY_ADDRESS)


#write.csv(d_WU_Fix2, "WU_Fix2.csv", row.names = FALSE)
#write.csv(d_IND_Fix, "IND_Fix.csv", row.names = FALSE)


## add City to facility name as part of matching

### check for matches using STATE+FacilityName or CUUNTY - CITY_ADDRESS
d_WU_Fix2$FACILITYID_CntyNAME <- d_IND_Fix$FACILITYID[match(d_WU_Fix2$COUNTY_NAME, d_IND_Fix$COUNTY_NAME)]

d_WU_Fix2$FACILITYID_CADD <- d_IND_Fix$FACILITYID[match(d_WU_Fix2$CITY_ADDRESS, d_IND_Fix$CITY_ADDRESS)]

d_WU_Fix2$FACILITYID_CntyADD <- d_IND_Fix$FACILITYID[match(d_WU_Fix2$COUNTY_ADDRESS, d_IND_Fix$COUNTY_ADDRESS)]

d_WU_Fix2$FACILITYID_CNAME <- d_IND_Fix$FACILITYID[match(d_WU_Fix2$CITY_NAME, d_IND_Fix$CITY_NAME)]

d_WU_Fix2$FACILITYID_StNAME <- d_IND_Fix$FACILITYID[match(d_WU_Fix2$STATE_NAME, d_IND_Fix$STATE_NAME)]


write.csv(d_WU_Fix2, "match_between_WU_v4IND_lists.csv", row.names = FALSE)


## create one combined FACILITYID field for joining

d_WU_Fix3 <- d_WU_Fix2 %>%
  mutate(FACILITYID_MATCH = case_when(
    !is.na(FACILITYID_CntyNAME) ~ FACILITYID_CntyNAME,
    (is.na(FACILITYID_CntyNAME) & !is.na(FACILITYID_CNAME)) 
        ~ FACILITYID_CNAME,
    (is.na(FACILITYID_CntyNAME) & is.na(FACILITYID_CNAME) & !is.na(FACILITYID_CADD)) 
        ~ FACILITYID_CADD,
    (is.na(FACILITYID_CntyNAME) & is.na(FACILITYID_CNAME) & is.na(FACILITYID_CADD) & !is.na(FACILITYID_CntyADD)) 
        ~ FACILITYID_CntyADD,
    (is.na(FACILITYID_CntyNAME) & is.na(FACILITYID_CNAME) & is.na(FACILITYID_CADD) & is.na(FACILITYID_CntyADD)) 
        ~ FACILITYID_StNAME
  ))


# pull matches using index numbers 
#join_IND <- d_IND[record_WU_IDX, ]

#test_IND <- join_IND %>% filter(!is.na(FACILITYID))

WU_IND_matches <- left_join(d_WU_Fix3, d_IND_Fix, 
                          by = c("FACILITYID_MATCH" = "FACILITYID"))

## subset to remove extra columns from previous match but need to check if any unique ones
##  that could be added back in
WU_IND_matches2 <- WU_IND_matches %>%
  subset(select = -c(SITESELECTION_FACILITYID, SITESELECTION_NAME, SITESELECTION_ADDRESS,
                     SITESELECTION_ADDRESS_Fix, SITESELECTION_ADRSFLAG, SITESELECTION_CITY,
                     SITESELECTION_STATE, SITESELECTION_COUNTY, SITESELECTION_ZIP,
                     SITESELECTION_FLAGZIP, SITESELECTION_PHONE, SITESELECTION_LATITUDE,
                     SITESELECTION_LONGITUDE, SITESELECTION_NAICS, SITESELECTION_NAICSDESCR,
                     SITESELECTION_SIC, SITESELECTION_SICDESCR, SITESELECTION_ZIP_PHONE,
                     SITESELECTION_CITY_ADDRESS, SITESELECTION_LL_Src, SITESELECTION_DATASET_MATCHES_ALL2,
                     SITESELECTION_NAICS_2, SITESELECTION_LATITUDE2, SITESELECTION_LONGITUDE2,
                     SITESELECTION_NAICS2_2))


write.csv(WU_IND_matches2, "match_between_WU_v4IND_lists.csv", row.names = FALSE)


################################################################################
######################## end of matching code ##################################
### summarize industry types without a match by name or by city+address

## first select IN records where match is NA

No_match <- WU_IND_matches %>%
  filter(is.na(SITESELECTION_FACILITYID) & is.na(FACILITYID) &
           Category == "IN")


# create 2- and 3-digit NAICS column to remove codes not equal to 31-33
no_match_Scnt$NAICS_2 <- as.numeric(substr(no_match_Scnt$NAICS.x, 
                                           start = 1, stop = 2))

no_match_Scnt$NAICS_3 <- as.numeric(substr(no_match_Scnt$NAICS.x, 
                                           start = 1, stop = 3))



no_match_Scnt2$NAICS_2 <- as.numeric(substr(no_match_Scnt2$NAICS.x, 
                                            start = 1, stop = 2))


## get distinct facility names

No_matchD <- No_match %>%
  distinct(State, FacilityName, City1, .keep_all = TRUE)


## count by NAICS/SIC codes and by NAICS/SIC and State

no_match_cnt <- No_matchD %>%
  group_by(NAICS.x) %>%
  summarize(NAICS_cnt = n()) %>%
  ungroup

no_match_cnt2 <- No_matchD %>%
  group_by(SIC.x) %>%
  summarize(SIC_cnt = n()) %>%
  ungroup

no_match_Scnt <- No_matchD %>%
  group_by(State, NAICS.x) %>%
  summarize(NAICS_cnt = n()) %>%
  ungroup

no_match_Scnt2 <- No_matchD %>%
  group_by(State, SIC.x) %>%
  summarize(SIC_cnt = n()) %>%
  ungroup


# create 2- and 3-digit NAICS column to remove codes not equal to 31-33
no_match_Scnt$NAICS_2 <- as.numeric(substr(no_match_Scnt$NAICS.x, 
                                              start = 1, stop = 2))

no_match_Scnt$NAICS_3 <- as.numeric(substr(no_match_Scnt$NAICS.x, 
                                           start = 1, stop = 3))



no_match_Scnt2$NAICS_2 <- as.numeric(substr(no_match_Scnt2$NAICS.x, 
                                           start = 1, stop = 2))


no_match_Scnt_IND <- no_match_Scnt %>%
  filter((NAICS_2 > 30 & NAICS_2 <34) |
           is.na(NAICS_2))


write.table(no_match_Scnt_IND, "NAICS_counts_by_state.txt", sep="\t", row.names=FALSE)


## also do counts by 3-digit NAICS

no_match_Scnt <- No_matchD %>%
  group_by(State, NAICS_3) %>%
  summarize(NAICS_cnt = n()) %>%
  ungroup



