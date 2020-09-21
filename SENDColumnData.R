# These functions work together with the SendDataFactory


getOrres <- function(aDomain,aSex,aTestCD,aSpec,aSpecies,aStrain){
  aDomainConfig <- getConfig(aDomain)
  ## If Domain is numeric
  if(aDomain %in% c("BG", "BW", "EG", "FW", "LB", "PC", "PP", "VS")){
    ## If config found
    if(exists("aDomainConfig") && !is.null(aDomainConfig)) {
      # print(paste(" Debug 1 Determining mean: ",aDomain," sex: ",aSex," testcd: ",aTestCD))
      testcd_ind <- str_which(names(aDomainConfig), "TESTCD")
      mean_ind <- str_which(names(aDomainConfig), "STRESM")
      sd_ind <- str_which(names(aDomainConfig), "STRESSD")
      aValueMean <- aDomainConfig[aDomainConfig$SEX == aSex &
                                    aDomainConfig[,testcd_ind] == aTestCD,
                                  mean_ind]
      aValueSD <- aDomainConfig[aDomainConfig$SEX == aSex &
                                    aDomainConfig[,testcd_ind] == aTestCD,
                                  sd_ind]
      # for some domains, check as well to species and strain level
      if(aDomain %in% c("BG", "BW", "EG", "FW", "LB","OM","VS")){
        aValueMean <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig$SPECIES == aSpecies & aDomainConfig$STRAIN == aStrain &
                                    aDomainConfig[,testcd_ind] == aTestCD,
                                  mean_ind]
        if (is.null(aValueMean)) {
          stop(paste("Unable to find configurated mean value for this species",aSpecies,"and strain",aStrain))
        }
        aValueSD <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig$SPECIES == aSpecies & aDomainConfig$STRAIN == aStrain &
                                  aDomainConfig[,testcd_ind] == aTestCD,
                                sd_ind]
      }
      aValue <- round(rnorm(1, aValueMean, aValueSD), digits=2)
      # print(paste(" DEBUG Domain: ",aDomain," sex: ",aSex," testcd: ",aTestCD))
      # print(paste("    DEBUG Test cd: ",testcd_ind," mean index and value: ",mean_ind,aValueMean,aValue))
      ## If config not found
    } else {
      aValue <- round(runif(1, 2.0, 100), digits=2)
    }
    ## If domain is catagorical
  } else {
    ## not numeric and If config is found:
    if(!is.null(aDomainConfig)) {
      # print(paste(" DEBUG using factor and proportion ",aTestCD,aSex,aSpecies,aStrain,aSpec))
      testcd_ind <- str_which(names(aDomainConfig), "TESTCD")
      fact_ind <- str_which(names(aDomainConfig), "(STRESC)$")
      prop_ind <- str_which(names(aDomainConfig), "(FREQ)$")
      spec_ind <- str_which(names(aDomainConfig), paste0(aDomain,"SPEC"))
      
      ## Pull proportions for this sex,testcd
      # check if config includes spec
      if (identical(spec_ind, integer(0))) {
        testConfig <- aDomainConfig[aSex==aDomainConfig$SEX &
                                      aTestCD==aDomainConfig[,testcd_ind],]
      } else {
        testConfig <- aDomainConfig[aSex==aDomainConfig$SEX &
                                      aTestCD==aDomainConfig[,testcd_ind] &
                                      aSpec==aDomainConfig[,spec_ind],]
      }
      # for some domains, check as well to species and strain level
      if(aDomain %in% c("MA", "MI", "CL")){
        if (identical(spec_ind, integer(0))) {
          testConfig <- aDomainConfig[aSex==aDomainConfig$SEX & aDomainConfig$SPECIES == aSpecies & aDomainConfig$STRAIN == aStrain &
                                        aTestCD==aDomainConfig[,testcd_ind],]
        } else {
          testConfig <- aDomainConfig[aSex==aDomainConfig$SEX & aDomainConfig$SPECIES == aSpecies & aDomainConfig$STRAIN == aStrain &
                                        aTestCD==aDomainConfig[,testcd_ind] &
                                        aSpec==aDomainConfig[,spec_ind],]
        }
      }                                      
      totalProportion <- sum(testConfig[,prop_ind])
      
      ## If Proportions don't sum to 1 the sample() function will normalize
      #if(totalProportion != 1) {
      #  warning(paste0("Total Proportion for: ",aTestCD," does not sum to 1: ", totalProportion,", Normalizing to 1"))
      #}
      
      # if none found, may be due to no available for this specimen, skip row
      if (nrow(testConfig)==0) {
        aValue <- "Skip Row"
        skipDataRow <<- TRUE
        # print("Debug - no value found")
      } else {  
        # print(paste(" DEBUG determining value ",aTestCD,prop_ind,fact_ind))
        aValue <- sample(testConfig[,fact_ind], size = 1, prob = testConfig[,prop_ind])
      }
      
      ## If config is not found
    } else {
      nameList <- getCodeList(paste(aDomain,"STRESC",sep=""))
      aValue <- CTRandomName(nameList)
    }
  }
  lastOrres <<- aValue 
  aValue
}

getOrresUnit <- function(aCol,aDomain,aSex,aTestCD,aSpecies,aStrain){
  # should be tied by configuration to the ORRES value
  aDomainConfig <- getConfig(aDomain)
  ## If config found
  if(exists("aDomainConfig") && !is.null(aDomainConfig)) {
      # print(paste(" Debug 1 Determining mean: ",aDomain," sex: ",aSex," testcd: ",aTestCD))
      unitInd <- str_which(names(aDomainConfig), "(STRESU)$")
      testcd_ind <- str_which(names(aDomainConfig), "TESTCD")
  } else {
    stop(paste("Unable to find configurated unit for this species",aSpecies,"and strain",aStrain))
  }
  # get unit from configuration matching sex
    aValue <- "Not yet set"
    # for some domains, check as well to species and strain level
    if(aDomain %in% c("BG", "BW", "EG", "FW", "LB","OM","VS")){
      aValue1 <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig$SPECIES == aSpecies & aDomainConfig$STRAIN == aStrain &
                                    aDomainConfig[,testcd_ind] == aTestCD,unitInd]
    } else {
      aValue1 <- aDomainConfig[aDomainConfig$SEX == aSex & aDomainConfig[,testcd_ind] == aTestCD,unitInd]
    }
    if (!is.null(aValue1)) {
      aValue <- aValue1
    }
  lastOrresu <<- aValue 
  aValue
}

getStresc <- function(aCol){
  # for now assume it is the same as the last orres
  lastOrres
}
getStresuUnit <- function() {
  # for now assume it is the same as the orresu
  lastOrresu  
}
# returns column data based upon the column name
getColumnData <- function (aCol,aSex,aTreatment,anAnimal,aRow,aDomain,aStudyID,aTestCD,iDay,aSpec,aSpecies,aStrain,aSENDVersion) {
  aData <- ""
  aSeqCol <- paste(aDomain,"SEQ",sep="")
  aTestCDCol <- paste(aDomain,"TESTCD",sep="")
  aTestCol <-paste(aDomain,"TEST",sep="")
  aORRESCol <- paste(aDomain,"ORRES",sep="")
  aSTRESCCol <-paste(aDomain,"STRESC",sep="")
  aSTRESNCol <- paste(aDomain,"STRESN",sep="")
  aORRESUCol <- paste(aDomain,"ORRESU",sep="")
  aSTRESUCol <- paste(aDomain,"STRESU",sep="")
  aSPECCol <- paste(aDomain,"SPEC",sep="")
  aDay <- paste(aDomain,"DY",sep="")
  aNOMDYCol <- paste(aDomain,"NOMDY",sep="")
  aNOMLBLCol <- paste(aDomain,"NOMLBL",sep="")
  aData <- NA
  # Next line for help in debugging
  # print(paste(" Debug 3 Getting column data for:",aCol,aSex,aTreatment,anAnimal,aRow,aDomain,aStudyID,aTestCD,aSpec),sep=":")
  if (aCol=="DOMAIN") aData <- aDomain
  if (aCol=="STUDYID") {aData <- aStudyID}
  if (aCol==aSeqCol) {aData <- aRow}
  if (aCol=="USUBJID") {aData <- paste(aStudyID,"-",anAnimal,sep="")
  }
  if (aCol==aTestCDCol) {
    aData <- getSENDTestCode(aCol,aTestCD)
  }
  if (aCol==aTestCol)  {aData <- getSENDLastTestCodeName(aCol,aDomain)}
  if (aCol==aORRESCol) aData <- getOrres(aDomain,aSex,aTestCD,aSpec,aSpecies,aStrain)
  if (aCol==aORRESUCol) {aData <- getOrresUnit(aCol,aDomain,aSex,aTestCD,aSpecies,aStrain)}
  if (aCol==aSTRESCCol) {aData <- getStresc(aCol)}
  if (aCol==aSTRESNCol) {aData <- suppressWarnings(as.numeric(lastOrres))}
  if (aCol==aSTRESUCol) {aData <- getStresuUnit()}
  if (aCol==aDay) {aData <- iDay}
  if (aSENDVersion=="3.0") {
    if (aCol=="VISITDY") {aData <- iDay}
  } else {
    if (aCol==aNOMDYCol) {aData <- iDay}
    if (aCol==aNOMLBLCol) {aData <- paste("Day",iDay)}
  }
  if (aCol==aSPECCol) aData <- aSpec
  # return the data
  # print(paste("               DEBUG aData returned: ",aData))
  aData
}
