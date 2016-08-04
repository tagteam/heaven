### getAdmLimits.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43) 
## Version: 
## last-updated: Aug  4 2016 (22:07) 
##           By: Thomas Alexander Gerds
##     Update #: 12
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' R-version of the sas code from hell.
##'
##' For now pattype is ignored. 
##' @title Thomas's first R-version of the sas code from hell 
##' @param dt data.table which provides variables: pnr, recno, inddto, uddto, pattype
##' @return nothing. but input data.table is modified such that all intermediate admission records are removed. This means that overlapping admission
##' intervals in the input data are combined into one admission.
##' @examples
##' data(samplepop)
##' ## take a copy to preserve the object samplepop 
##' pop=copy(samplepop)
##' setDT(pop)
##' newpop=getAdmLimits(pop)
##' print(print(newpop))
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
getAdmLimits <- function(dt){
    stopifnot(is.data.table(dt))
    wdt=copy(dt)
    ## sort by pnr and admission dates 
    setkey(wdt,pnr,inddto)
    ## setkey(wdt,pnr,inddto,uddto)
    ## translate variable names
    setnames(wdt,c("inddto","uddto"),c("admdate","dischargedate"))
    ## get discharge date (uddto) of previous record by pnr
    wdt[,prev.dischargedate:= c(-9,dischargedate[-.N])]
    wdt[unique(wdt[,"pnr",with=FALSE]),prev.dischargedate:=-9,mult="first"]
    ## remove intermediate records 
    wdt <- wdt[dischargedate>prev.dischargedate]
    ## refresh prev.dischargedate
    wdt[,prev.dischargedate:=c(-9,dischargedate[-.N])]
    wdt[unique(wdt[,"pnr",with=FALSE]),prev.dischargedate:=-9,mult="first"]
    ## mark start of new admissions within pnr
    wdt[,startadm:=1*(admdate > prev.dischargedate),by=pnr]
    wdt[,overlap:=cumsum(startadm),by=pnr]
    ## set discharge date of first record to maximal discharge date within 
    ## overlapping admissions
    wdt[,dischargedate:=max(dischargedate),by=c("pnr","overlap")]
    ## remove intermediate overlapping admissions
    wdt <- wdt[startadm==1]
    ## remove unnecessary variables
    wdt[,c("prev.dischargedate","startadm","overlap"):=NULL]
    wdt
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here
