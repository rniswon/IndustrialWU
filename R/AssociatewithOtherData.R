

merge_nationaldata <- function(nonSWUDS, national_Xwalks, datacodes_Xwalks, natdata = list()) {
  natHeaders <- list(
    HeaderCrosswalk = get_filledcsv(file.path(national_Xwalks, "HeaderCrosswalk.csv")),
    DataCodesCrosswalk = datacodes_Xwalks
    )
 natData <- readandrename_columns(natdata, natHeaders, national_Xwalks, data = "National") %>%
   reformat_data(., natHeaders, national_Xwalks, data = "National") %>%
   filter(!is.na(FacilityName)) %>% mutate(State = State1)
 
 nonSWUDSwNat <- merge_andreplaceNA(nonSWUDS, natData) |> 
   dplyr::select(State, any_of(names(natHeaders$HeaderCrosswalk)))
 
 return(nonSWUDSwNat)

}
