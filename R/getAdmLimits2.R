### getAdmLimits2.R --- 
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

getAdmLimits2 <- function(dt2){
    stopifnot(is.data.table(dt2))
    wdt2=copy(dt2)
    ##Converts Date to integer
    if (is.integer(wdt2$inddto)){
      datechecker=FALSE
    }else{
      wdt2$inddto <- as.integer(wdt2$inddto)
      wdt2$uddto <- as.integer(wdt2$uddto)
      datechecker=TRUE
    }  
      ## sort by pnr and admission dates 
    setkey(wdt2,pnr,inddto)
    ## latest admission date by pnr
    wdt2[,max.uddto:=cummax(uddto),by=pnr]
    ##start of each admission
    wdt2[,startadm:=1L*(inddto>shift(max.uddto)),by=pnr]
    wdt2[.(unique(pnr)),startadm:=1L,mult="first"]
    ## overlapping admissions
    wdt2[,startadm:=cumsum(startadm),by=pnr]
    ##start and end date for the hospitalization the admission is part of.
    wdt2[,':='( startadm=inddto[1] , max.uddto=max.uddto[.N] ) ,by=.(pnr,startadm)] 
    ##Change names to what the two variables now contain
    wdt2[,odd:=ifelse(uddto<inddto | uddto-inddto>356.26 | uddto>as.integer(Sys.Date())|inddto>as.integer(Sys.Date()),1,0)]
    setnames(wdt2,c("startadm","max.uddto"),c("first.inddto","last.uddto"))
    ## Convert if input were format Date
    if(datechecker==TRUE){
      wdt2$uddto <- as.Date(wdt2$uddto,origin="1970-01-01")
      wdt2$inddto <- as.Date(wdt2$inddto,origin="1970-01-01")
      wdt2$last.uddto <- as.Date(wdt2$last.uddto,origin="1970-01-01")
      wdt2$first.inddto <- as.Date(wdt2$first.inddto,origin="1970-01-01")
    }
    wdt2
}

#----------------------------------------------------------------------
### getAdmLimits2.R ends here
