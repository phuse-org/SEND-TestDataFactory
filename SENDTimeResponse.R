# These functions work together with the SendDataFactory


getTimeResponse <- function(aDomain,aSex,aTestCD,aSpec,aSpecies,aStrain,iDay){
## Time response difference returned based upon domain
  aValue <- 0
  if (aDomain=="BW") {
    aDomainConfig <- getConfig("BG")
    ## If config found
    if(exists("aDomainConfig") && !is.null(aDomainConfig)) {
      printDebug(paste(" Debug 1 Determining mean: ",aDomain," sex: ",aSex," testcd: ",aTestCD))
      testcd_ind <- str_which(names(aDomainConfig), "TESTCD")
      mean_ind <- str_which(names(aDomainConfig), "STRESM")
      sd_ind <- str_which(names(aDomainConfig), "STRESSD")
        aValueMean <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig$SPECIES == aSpecies,
                                    mean_ind]
        if (is.null(aValueMean)) {
          stop(paste("Unable to find configurated mean value for this species",aSpecies,"and sex",aSex))
        }
        aValueSD <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig$SPECIES == aSpecies,
                                  sd_ind]
      aValue <- round(rnorm(1, aValueMean, aValueSD), digits=2)
      # above is gain over entire study period, now prorate based upon length of the study
      studyLength <- getStudyLength()
      # print(paste("Debug 2 Study length is:", studyLength,"so add to this BW",aValue))
      aValue <- round(aValue * iDay/studyLength,digits=2)
      # print(paste("Debug 3 Study length is:", studyLength,"so add to this BW",aValue))
    }  # check on existance of domain configuration
  }
  # return the difference to add to the result so that it increases or decreases
  aValue
}
  
