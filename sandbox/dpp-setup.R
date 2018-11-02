### dpp-setup.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 15 2018 (09:30) 
## Version: 
## Last-Updated: Nov  1 2018 (15:02) 
##           By: Thomas Alexander Gerds
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(heaven)
library(data.table)
pop <- simPop(100)
lpr <- simAdmissionData(10000)
lmdb <- simPrescriptionData(10000)


Source(heaven)

x <- dpp()
addData(x,name="pop") <- pop
addData(x,name="lmdb") <- lmdb
addData(x,name="lpr") <- lpr
addInclusion(x,name="firstAF") <- firstAdmission(data=lpr,var="diag",expression="DM62",sortkey="uddto",by="pnr")
addVariable(x,name="gender",target="baseline") <- function(pop){function(pop)pop$sex}
x <- do(x,nobs=10)


addVariable(x,name="Asacol",target="baseline") <- function(data=lmdb){}


list(..., bsl=data.table(pop[inclusion.criteria]$sex),
     pop[inclusion.criteria]$age.at.bsl)

######################################################################
### dpp-setup.R ends here
