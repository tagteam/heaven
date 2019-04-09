### guessRawdataDirectory.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Apr  9 2019 (15:30) 
## Version: 
## Last-Updated: Apr  9 2019 (15:30) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
guessRawdataDirectory <- function(){
    path <- strsplit(getwd(),"/")[[1]]
    this <- grep("70[0-9]+[0-9]+[0-9]+[0-9]+",path,value=TRUE)
    if (length(this)==0)stop("Cannot guess raw data directory based on current working directory.\nPlease specify directory or set working directory to workdata.")
    else{
        dir <- paste0("x:/data/rawdata_hurtig/",this)
    }
    dir
}



######################################################################
### guessRawdataDirectory.R ends here
