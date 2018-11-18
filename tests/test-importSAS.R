### test-importSAS.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  2 2018 (07:57) 
## Version: 
## Last-Updated: Nov  2 2018 (08:56) 
##           By: Thomas Alexander Gerds
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
## this file can only run when sas is installed. currently it only works in
## tag's file system on doob:
library(testthat)
library(heaven)
library(data.table)
context("importSAS")

sas.file <- system.file("testdata/lmdb.sas7bdat", package="heaven")
sas.exists <- (!(Sys.which("sas")=="") | !(Sys.which("C:/Program Files/SASHome/SASFoundation/9.4/sas.exe")==""))
if(!sas.exists){
    cat("SAS not available so skipping SAS tests.")
}else{
    test_that("importSAS loads test dataset",{
        testA <- try(importSAS(file=sas.file,obs=10))
        testB <- try(importSAS(file=sas.file,obs=10,date.vars="eksd"))
        testC <- try(importSAS(file=sas.file,
                               obs=10,
                               colClasses=list(character=c("pnr","packsize"),"numeric"="apk","factor"="eksd")))
    })
}



######################################################################
### test-importSAS.R ends here
