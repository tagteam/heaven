### dpp-setup.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 15 2018 (09:30) 
## Version: 
## Last-Updated: Nov  5 2018 (06:19) 
##           By: Thomas Alexander Gerds
##     Update #: 43
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(heaven)
library(profvis)
library(profmem)
library(data.table)
library(devtools)
library(Rcpp)

## a <- readRDS("~/tmp/a.rds")
## sourceCpp("~/research/SoftWare/heaven/src/innerMedicinMacro.cpp")
## do.call("innerMedicinMacro",a)

pop <- simPop(100)
lpr <- simAdmissionData(1000)
lmdb <- simPrescriptionData(1000)

Source(heaven)

x <- dpp()
addData(x,name="pop") <- pop
addData(x,name="lmdb") <- lmdb
addData(x,name="lpr") <- lpr
## addInclusion(x,name="firstAF") <- firstAdmission(data=lpr,var="diag",expression="DM9|DM6",sortkey="uddto",by="pnr")
addInclusion(x,name="firstAF") <- selector(data="lpr",var="diag",search.term="DM",sortkey="uddto",by="pnr")
addVariable(x,name="N07",target="baseline") <- selector(data="lmdb",var="atc",search.term="N07",sortkey="eksd",by="pnr")
addVariable(x,name="gender",target="baseline") <- selector(data="pop",var="sex",search.term=NULL,sortkey=NULL,by=NULL,select="all")
X <- do(x,nobs=10)

addVariable(x,name="Asacol",target="baseline") <- function(data=lmdb){}


list(..., bsl=data.table(pop[inclusion.criteria]$sex),
     pop[inclusion.criteria]$age.at.bsl)

######################################################################
### dpp-setup.R ends here
