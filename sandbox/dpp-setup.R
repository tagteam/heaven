### dpp-setup.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 15 2018 (09:30) 
## Version: 
## Last-Updated: Nov 27 2018 (18:47) 
##           By: Thomas Alexander Gerds
##     Update #: 80
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

set.seed(17)
pop <- simPop(100)
lpr <- simAdmissionData(1000)
lmdb <- simPrescriptionData(1000)


Source(heaven)
x <- dpp()
addData(x,name="pop") <- pop
addData(x,name="lmdb") <- lmdb
addData(x,name="lpr") <- lpr
## addInclusion(x,name="firstAF") <- firstAdmission(data=lpr,var="diag",expression="DM9|DM6",sortkey="uddto",by="pnr")
addInclusion(x,name="firstAF") <- selector(data="lpr",name=c("DM","dateDM"),var="diag",search.term="DM",sortkey="uddto",by="pnr",select="unique.pnr")
addExclusion(x,name="DE") <- selector(data="lpr",var="diag",search.term="^DE",sortkey="uddto",by="pnr",name="unique.pnr")
addVariable(x,name="N07first",target="baseline") <- selector(data="lmdb",name="N07.first",var="atc",search.term="N07",select="first",sortkey="eksd",by="pnr")
addVariable(x,name="N07last",target="baseline") <- selector(data="lmdb",name="N07.last",var="atc",search.term="N07",select="last",sortkey="eksd",by="pnr")
addVariable(x,name="Demo",target="baseline") <- selector(data="pop",name="sex.age",var=NULL,search.term=NULL,sortkey=NULL,by=NULL,select="all",collect=c("sex","birthdate"))
X <- process(x,n=300)

X$baseline



######################################################################
### dpp-setup.R ends here
