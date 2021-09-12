# These functions work together with the SendDataFactory


# 
skipRow <- function(aTestCD,iDay,endDay) {
  aResult <- FALSE
  # skip terminal body weight except on last day
  if (aTestCD=="TERMBW" && (iDay != endDay)) {
    aResult <- TRUE
  }
  aResult
}
#

# set description labels for each field 
setLabels <- function(theDomain, theDataset) {
  printDebug(paste("             In setLabels"))
  theColumns <- labels(theDataset)[2]
  printDebug(paste("             In setLabels, columns are",theColumns))
  for (i in theColumns) {
  for(aColumn in i){
    aDescription <- dfSENDIG[dfSENDIG$Column==aColumn & dfSENDIG$Domain==theDomain,]$Label
    Hmisc::label(theDataset[,aColumn]) <- aDescription
    printDebug(paste("             In setLabels",aColumn,aDescription))
    }
  }
  theDataset
}

removeColumns <- function(theDataSet) {
  # if all NA, remove empty columns
  theDataSet[theDataSet=="NA"] <- ""
  emptycols <- sapply(theDataSet, function (k) all(is.na(k)))
  theDataSet <- theDataSet[!emptycols]
  theDataSet
}

# Sort the domain and resequence
sortDomain <- function(aDomain,aDataset) {
  switch(aDomain,
         "BW" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,BWTESTCD,BWDY)
           aDataset$BWSEQ <- 1:nrow(aDataset) 
         },
         "CL" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,CLTESTCD,CLDY)
           aDataset$CLSEQ <- 1:nrow(aDataset) 
         },
         "LB" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,LBTESTCD,LBDY)
           aDataset$LBSEQ <- 1:nrow(aDataset) 
         },
         "FW" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,FWTESTCD,FWDY)
           aDataset$FWSEQ <- 1:nrow(aDataset) 
         },
         "EG" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,EGTESTCD,EGDY)
           aDataset$EGSEQ <- 1:nrow(aDataset) 
         },
         "MA" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,MASPEC,MATESTCD,MADY)
           aDataset$MASEQ <- 1:nrow(aDataset) 
         },
         "MI" = {
           printDebug(paste(" In sortDomain about to sort"))  
           aDataset <- arrange(aDataset,STUDYID,USUBJID,MISPEC,MITESTCD,MIDY)
           printDebug(paste(" In sortDomain sorted, now renumbering"))  
           aDataset$MISEQ <- 1:nrow(aDataset) 
         },
         "OM" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,OMSPEC,OMTESTCD,OMDY)
           aDataset$OMSEQ <- 1:nrow(aDataset) 
          },
         "PC" = {
           aDataset <- arrange(aDataset,STUDYID,USUBJID,PCTESTCD,PCDY,PCTPTNUM)
           aDataset$PCSEQ <- 1:nrow(aDataset) 
          },
         "PP" = { 
           aDataset <- arrange(aDataset,STUDYID,USUBJID,PPTESTCD)
           aDataset$PPSEQ <- 1:nrow(aDataset) 
         } 
  )
  aDataset
}

# Get specimins for some domains
getSpecs <- function(aDomain,aSex,aTestCD,aSpecies,aStrain) {
  switch(aDomain,
         "MI" = {aConfig <- getConfig("MI")},
         "MA" = {aConfig <- getConfig("MA")},
         "OM" = {aConfig <- getConfig("OM")},
         "PC" = {aConfig <- getConfig("PC")},
         "PP" = {aConfig <- getConfig("PP")}
  )
  aList <- list()  
  if (exists("aConfig")) {
    # print("Obtaining specimen list")
    # read from configuration file
    ## Pull proportions for this sex,testcd
    testcd_ind <- str_which(names(aConfig), "TESTCD")
    strain_ind <- str_which(names(aConfig), "STRAIN")
    species_ind <- str_which(names(aConfig), "SPECIES")
    # on some, go to species and strain as well
    if (identical(strain_ind, integer(0))) {
      testConfig <- aConfig[aSex==aConfig$SEX &
                                    aTestCD==aConfig[,testcd_ind],]
    } else {
      testConfig <- aConfig[aSex==aConfig$SEX &
                                    aTestCD==aConfig[,testcd_ind] &
                                    aSpecies==aConfig[,species_ind] &
                                    aStrain==aConfig[,strain_ind],]
    }
    spec_ind <- str_which(names(aConfig), paste0(aDomain,"SPEC"))
    theSpecs <- unique(testConfig[,spec_ind])
    aList <- as.list(theSpecs)
  } else {
    # print("No specimen list")
    aList <- append(aList,"No Specimen")  
  }
unique(aList)
}

# from configuration, get column based upon incoming column (like testcd to test)
getMatchColumn <- function(aDomain,aColumn1,aValue1,aColumn2) {
  configFiles <- list.files("configs")
  aConfig <- getConfig(aDomain)
  #print(paste("Matching columns from ",aColumn1,aValue1,aColumn2))
  # FIXME might need other discriminating factors like Sex, Species,...
  # get first matching one
  answer <- aConfig[aConfig[aColumn1]==aValue1,][aColumn2][1,1]
  # print(paste("        answer",answer))
  answer
}

# get time point from configuration file
getTPTNUM <- function(aDomain,aSex,aTestCD,aSpec,aSpecies,aTime) {
  aDomainConfig <- getConfig(aDomain)
  tptnumInd <- str_which(names(aDomainConfig), "TPTNUM")
  eltmInd <- str_which(names(aDomainConfig), "ELTM")
  testcdInd <- str_which(names(aDomainConfig), "TESTCD")
  if (identical(tptnumInd, integer(0))) {
    # if no timepoint
    aResult <- ""
  }  else {
    # get the test code
    aResult <- aDomainConfig[aDomainConfig$SEX == aSex & 
                  aDomainConfig[,testcdInd] == aTestCD & 
                  aDomainConfig[,eltmInd] == aTime,tptnumInd][1]
    printDebug(paste("Time point num obtained:", aResult))
  }
  aResult
}

# get time point from configuration file
getTPT <- function (aDomain,aSex,aTestCD,aSpec,aSpecies,aTime) {
  printDebug("To calculate: Time point obtained")
  aDomainConfig <- getConfig(aDomain)
  tptInd <- str_which(names(aDomainConfig), "(TPT)$")
  eltmInd <- str_which(names(aDomainConfig), "ELTM")
  testcdInd <- str_which(names(aDomainConfig), "(TESTCD)$")
  if (identical(tptInd, integer(0))) {
     # if no timepoint
    aResult <- ""
  } else {
    printDebug(paste("Calculating:",tptInd,eltmInd,testcdInd))
    printDebug(aDomainConfig)
    # get the test code
    aResult <- aDomainConfig[aDomainConfig$SEX == aSex &
                               aDomainConfig[,testcdInd] == aTestCD & 
                               aDomainConfig[,eltmInd] == aTime,tptInd][1]
    printDebug(paste("Time point obtained:", aResult))
    
  }
  aResult
}

# get times from configuration file
getConfigTimes <- function (aDomain) {
  # get unique list of elapsed times
  aTimes <- unique(PCconfig$PCELTM)
}

#check if data frame has a DY component
hasDays <- function(aDF,aDomain) {
  aDY <- paste(aDomain,"DY",sep="")
  aResult <- (aDY %in% labels(aDF)[2][[1]] )
  aResult
}

createAnimalDataDomain <- function(input,aDomain,aDescription,aDFName) {
  theColumns <- dfSENDIG[dfSENDIG$Domain==aDomain,]$Column
  theLabels <- dfSENDIG[dfSENDIG$Domain==aDomain,]$Label
  # Creating the data fames
  printDebug(paste0("Creating the data frames with columns: ",theColumns))
  aDF <<- setNames(data.frame(matrix(ncol = length(theColumns), nrow = 1)),
                     theColumns
  )
  # set other global variables for use
  theTestArticle <<- input$testArticle
  aRow <- 1
  # set some defaults
  if (is.null(input$sex)) {
    sexList <- c("M","F")
  } else {
    sexList <- input$sex
  }
    
  if (is.null(input$treatment)) {
    treatmentList <- c("Control Group")
  }  else {
    treatmentList <- input$treatment
  }
  if (is.null(input$animalsPerGroup)) {
    animalsList <- 10
  }  else {
    animalsList <- input$animalsPerGroup
  }
  
  printDebug(paste("Looping by SEX:",sexList))
  for (aSex in sexList) {
  printDebug(paste("For this sex:",aSex))
  # loop over the tests for this domain
  aCodes <- getTestCDs(aDomain, aSex,input$species, input$strain)
  printDebug(aCodes)
  for(i in 1:nrow(aCodes)) {
    aTestCD <-  as.character(aCodes[i,])
    aSpecs <- getSpecs(aDomain,aSex,aTestCD,input$species, input$strain)
    printDebug(paste("  For this test code:",aTestCD))
    if (!skipRow(aTestCD,iDay,endDay)) {
      for(iSpec in 1:length(aSpecs)) {
        skipDataRow <<- FALSE
        aSpec <- as.character(aSpecs[iSpec])
        printDebug(paste("    For this specimen:",aSpec))
             # now loop on all groups
          printDebug(paste("Looping by treatment:",treatmentList))
          for (iTreatment in 1:length(treatmentList)) {
            aTreatment <- treatmentList[iTreatment]
            printDebug(paste("For this treatment:",aTreatment))
            # now loop on all animals for which we want to create rows
            printDebug(paste("Looping by animals per group:",animalsList))
            for (anAnimalInGroup in 1:animalsList) {
              # if this domain has days, loop over days
              # create animal number with treament and sex characters
              anAnimal <- paste0(iTreatment,aSex,anAnimalInGroup)
              printDebug(paste("For this animal:",anAnimal))
              # times per day for some domain
              aTimes <- "NA"
              if (hasDays(aDF,aDomain)) {
                # FIXME - use study length from configuration or user selection
                startDay <- 1
                endDay <- 8
              } else {
                startDay <- 1
                endDay <- 1  
              }
              # for Lab, just 2 days of data, FIXME to other choices
              if (aDomain=="LB") {
                startDay <- 1
                endDay <- 2
              }
              # for PC and PP, just 1 day of data, day 1
              if (aDomain=="PC") {
                startDay <- 1
                endDay <- 1
                # and read time list from configuration
                aTimes <- getConfigTimes(aDomain)
              }
              
              if (aDomain=="PP") {
                startDay <- 1
                endDay <- 1
              }

              # for OM, MA and MI, just 1 day of data, last day of study
              if (aDomain=="MA" || aDomain=="MI" || aDomain=="OM") {
                startDay <- 10
                endDay <- 10
              }
              # loop over day
              for (iDay in startDay:endDay) {
                printDebug(paste("For this day:",iDay))
                  for (aTime in aTimes) {
                    printDebug(paste("    For this time:",aTime))
                    printDebug(paste(" About to create row animal for",aTestCD, iDay, anAnimalInGroup, aTreatment, aSex,aSpec,input$species, input$strain, aTime))
                    aRowList <<- createRowAnimal(aSex,aTreatment,anAnimal,aDF,aRow,aDomain,
                    input$studyName,aTestCD,iDay,aSpec,input$species, input$strain , input$SENDVersion,aTime,iTreatment)
                    #
                    # replace empties with NA
                    aRowList <<- sub("$^", NA, aRowList)
                    printDebug(paste(" inserting",aRowList))
                    if (aRow>1) {
                      printDebug(paste("   after",aDF[aRow-1,]))
                    }
                    if (!skipDataRow) {
                      aDF[aRow,] <<- aRowList        
                      aRow <- aRow + 1
                    } # end of skip data row check
                  } # end of loop on time per day
              } # end of day loop
            } # end of animal loop
          } # end of treament loop
      } # end of loop on specimen
    } # end of skipRow check
   } # end of test loop
  } # end of sex loop
  aDF
}

createRowAnimal <- function(aSex,aTreatment,anAnimal,aDF,aRow,aDomain,aStudyID,
                            aTestCD,iDay,aSpec,aSpecies,aStrain,aSENDVersion,aTime,theArm) {
 # create to hold the row
 aList <- vector(mode = "list", length = 100)
 printDebug(paste("Creating row for:",aSex,aTreatment,anAnimal,aRow,aDomain,aStudyID,aTestCD,aSpec,aSpecies,aStrain))
 # loop on fields in data frame
 iCol <- 1
 for (aCol in labels(aDF)[2][[1]]) {
   # add value to the list of column values, based upon the column name
   columnData <- getColumnData(aCol,aSex,aTreatment,anAnimal,aRow,aDomain,
                                aStudyID,aTestCD,iDay,aSpec,aSpecies,aStrain,aSENDVersion,aTime,theArm)
   aList[[iCol]] <- columnData
   iCol <- iCol + 1
 }
 # truncate to correct size
 # return the list of fields
 aList[1:iCol-1]
}

setAnimalDataFiles <- function(input) {
    # Make a list of domains to handle
    DomainsList <- input$testCategories
    print(DomainsList)
    # create data frame based on structure
    # Loop on num domains
    index <- 0
    for (aDomain in DomainsList) {
      # timer set
      aTimer <- proc.time()
      index <- index + 1
      percentOfList <- index/length(DomainsList)
      setProgress(value=percentOfList,message=paste('Producing dataset: ',aDomain))
      aDFName <- paste(tolower(aDomain),"Out",sep="")
      aDescription <- paste(aDomain,"domain") 
      #FIXME - read description from SENDIG
      aDFReturned <<- createAnimalDataDomain(input,aDomain,aDescription,aDFName)
      # some variables calculated based upon items set
      aDFReturned <<- addCalcColumns(aDFReturned,aDomain)
      # remove permissible columns that are empty
      aDFReturned <<- aDFReturned[, checkCore(aDFReturned)]
      # next line not needed, as above line removes permissible columns that are empty
      # aDFReturned <<- removeColumns(aDFReturned)
      # sort to get a desired output order
      aDFReturned <<- sortDomain(aDomain,aDFReturned)
      # set numeric needs to happen after sort recreates sequence
      aDFReturned <<- setSENDNumeric(aDFReturned)
      aDFReturned <<- setLabels(aDomain,aDFReturned)
      # now reset the name of this dataframe to keep it
      assign(aDFName, aDFReturned, envir=.GlobalEnv)
      addToSet(aDomain,aDescription,aDFName)
      # showing timer results
      print(paste("Time to create",aDomain," is:"))
      print(proc.time() - aTimer)
      
    }
}
