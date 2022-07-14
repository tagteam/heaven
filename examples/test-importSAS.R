### test-importSAS.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  2 2018 (07:57) 
## Version: 
## Last-Updated: Jul 13 2022 (16:12) 
##           By: Thomas Alexander Gerds
##     Update #: 20
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
if (try(setwd("~/research/SoftWare/heaven/"))){
sas.file <- system.file("sandbox/lmdb.sas7bdat", package="heaven")
## sas.file <- "~/research/SoftWare/heaven/sandbox/lmdb.sas7bdat"

sas.exists <- (!(Sys.which("sas")=="") | !(Sys.which("C:/Program Files/SASHome/SASFoundation/9.4/sas.exe")==""))
if(!sas.exists){
    cat("SAS not available so skipping SAS tests.")
}else{
    test_that("importSAS loads test dataset",{
        testA <- try(importSAS(file=sas.file,obs=1000))
        filterA <- data.table(pnr=c("1","8"))
        filterA2 <- data.table(pnr=c(1,8))
        testAf <- try(importSAS(file=sas.file,obs=1000,filter=filterA2,save.tmp=0))        
        testB <- try(importSAS(file=sas.file,obs=10,date.vars="eksd"))
        testC <- try(importSAS(file=sas.file,
                               obs=10,
                               colClasses=list(character=c("pnr","packsize"),"numeric"="apk","character"="eksd")))
        testD <- try(importSAS(file=sas.file,obs=113,date.vars="eksd",keep="eksd",character.vars="strnum"))
        testE <- try(importSAS(file=sas.file,
                               obs=113,
                               date.vars="EksD",
                               keep="eksd",
                               numeric.vars=c("PNR"),
                               character.vars=c("strnum")))
        testF <- try(importSAS(file=sas.file,
                               obs=113,
                               date.vars="EksA",
                               keep="eksd",
                               numeric.vars=c("PNR"),
                               character.vars=c("strnum")))
    })
}
}


######################################################################
### test-importSAS.R ends here
