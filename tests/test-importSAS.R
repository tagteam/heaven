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
library(heaven)
Source(heaven)
testA <- try(importSAS(file="~/research/SoftWare/heaven/sandbox/lmdb.sas7bdat",obs=10))
testB <- try(importSAS(file="~/research/SoftWare/heaven/sandbox/lmdb.sas7bdat",obs=10,date.vars="eksd"))
testC <- try(importSAS(file="~/research/SoftWare/heaven/sandbox/lmdb.sas7bdat",
                       obs=10,
                       colClasses=list(character=c("pnr","packsize"),"numeric"="apk","factor"="eksd")))


######################################################################
### test-importSAS.R ends here
