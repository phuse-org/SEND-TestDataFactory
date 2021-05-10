### Functions to create and read SENDIG data.frame

# read all domain structures
readDomainStructures <-function() {
  # read if it is not there yet
  if (!exists("bSENDIGRead")) {
    bSENDIGRead <<- FALSE
  } 
  if (!bSENDIGRead) {
      IGFile <- "sendig-3-1-excel.xls"
      IGDownloadsDir <- paste0(sourceDir, "/downloads/")
      
      if(file.exists(paste0(IGDownloadsDir,IGFile))) {
        print(paste0("IG Loading... from ",IGDownloadsDir, IGFile))
      } else {
        print("IG Downloading...")
        # Create directory if not there
        createOutputDirectory(sourceDir,"downloads")
        # Reads from phuse github biocelerate location
        path <- "https://github.com/phuse-org/BioCelerate/raw/a9022106134943bd46e06d064b629b6ef3488125/metadata/sendig-3-1-excel.xls"
        CTxl <- paste0(IGDownloadsDir,IGFile)
        print(paste0("Downloading the IG file... ",path))
        GET(path, write_disk(CTxl),timeout(40))
      }
      df <- readWorksheet(loadWorkbook(paste0(IGDownloadsDir, IGFile)), sheet = "SENDIG 3.1 Variables")
      # column renames
      df <- rename(df, "Domain" = "Domain.Prefix")
      df <- rename(df, "Column" = "Variable.Name")
      df <- rename(df, "Label" = "Variable.Label")
      df <- rename(df, "Codelist" = "Controlled.Terms..Codelist.or.Format")
      df <- rename(df, "Expectancy" = "Core")
      dfSENDIG <<- df
      bSENDIGRead <<- TRUE
  }
}

# This function checks columns to verify if the column needs to be included
# based upon the columns core requirement. Returns a logical vector. TRUE
# if it should be included, FALSE otherwise
# TODO: This isn't very effiecent
checkCore <- function(dataset) {
  
  # Create vector for permisable columns
  domain_i <- unique(dataset$DOMAIN)
  cores <- dfSENDIG[dfSENDIG$Domain == domain_i, "Expectancy"]
  isPerm <- cores == "Perm"
  
  # Create TF vector for blank cols
  blankCol <- apply(dataset, 2, function(x) all(is.na(x)))
  
  # Return vector where both conditions are true.
  !(isPerm & blankCol)
}

# Reset within the dataframe as numeric columns that should be numeric
setSENDNumeric <- function(dataset) {
  domain_i <- unique(dataset$DOMAIN)
  numerics <- dfSENDIG[dfSENDIG$Domain == domain_i & dfSENDIG$Type=="Num", "Column"]
  numerics <- intersect(numerics,names(dataset))
  printDebug(paste(" In setSENDNumeric set numeric for these variables...",numerics))  
  printDebug(numerics)
  if (length(numerics)>1) {
    dataset[numerics] <- suppressWarnings(sapply(dataset[numerics],as.numeric))    
  } else if (length(numerics)==1) {
    dataset[numerics] <- as.numeric(dataset[[numerics]])
  }
  dataset
}


createOutputDirectory <- function (aDir,aStudy) {	
  setwd(aDir)
  if (file.exists(aStudy)){
    setwd(file.path(aDir, aStudy))
  } else {
    dir.create(file.path(aDir, aStudy))
    setwd(file.path(aDir, aStudy))
  }
}

sleepSeconds <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
