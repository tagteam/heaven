### getAdmLimits.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43)
## Version:
<<<<<<< HEAD
## last-updated: Aug  4 2016 (22:07)
##           By: Thomas Alexander Gerds
##     Update #: 12
=======
## last-updated: Aug  8 2016 (22:07)
##           By: CTP
##     Update #: 13
>>>>>>> d74773d319b96ec1c78628449bc5bffa92272e8a
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Adds first admission date and last discharge date to a sequence of admissions with diagnoses. The expected input data.table is the current standard
##' by which diagnoses and admissions are available in Statistics Denmark - a merge between diagnoses and the administrative data. The function is general
##' for all diagnoses and the examples show how e.g. the first infarction and the associated "true" admission and discharge ar obtained.
##'
##' For now pattype is ignored.
##' @title Thomas's first R-version of the sas code from hell
##' @param dt data.table which provides variables: pnr, recno, inddto, uddto, pattype. The function assumes these variable names that can be changed
##' with the function call when appropriate
##' @Details
##' dt is a data table with the following fields: inddto, uddto, pattype
##' if e.g. inddto has a different name, then the function call should include 'inddto="newname"'
##' @return
##' The same data.table as for input but with 3 new variables first.inddto, last.uddto and admnum (admission number)
##' @examples
##' data(samplediag)
##' ## take a copy to preserve the object samplepop
##' pop=copy(samplediag)
##' setDT(pop)
##' newpop=getAdmLimits(pop,admtype=c(0,1)) # Keep overnight admissions and emergency room admissions
##' print(print(newpop))
##' #As an example - get first AMI admission with limits
##' setkey(newpop,pnr,admnum)
##' newpop[grepl('I21',diag)|substr(diag,1,3)=='410',.(AMIdto=min(inddto),first.ind=min(first.inddto),last.ud=min(last.uddto)),by=list(pnr,admnum)][]
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
    getAdmLimits <- function(dt,admtype=0,pnr="pnr",inddto="inddto",uddto="uddto",pattype="pattype"){
    stopifnot(is.data.table(dt))
    ## Select relevant pattype values
    wdt <- dt[pattype %in% admtype]
    ## sort by pnr and admission dates
    setkey(wdt,pnr,inddto,uddto)
    ## get previous discharge (uddto) by pnr
    wdt <- wdt[,':='(p.uddto=c(-9,uddto[-.N]),p.inddto=c(-9,inddto[-.N]))]
    wdt <- wdt[unique(wdt[,"pnr",with=FALSE]),':='(p.uddto=-9,p.inddto=-9),mult="first"]
    ## get max relevant prior discharge to account for interpolated admissions
    wdt <- wdt[,max.uddto:=ifelse(inddto>=p.inddto & uddto<p.uddto & p.uddto != -9,p.uddto,uddto)]
    ## get prior max.uddto
    wdt <- wdt[,':='(p.max.uddto=c(-9,max.uddto[-.N]))]
    wdt <- wdt[unique(wdt[,"pnr",with=FALSE]),':='(p.max.uddto=-9),mult="first"]
    ## New admission?
    wdt <- wdt[,new.adm:=as.numeric(p.uddto==-9 | inddto>p.max.uddto)]
    # Admission number per pnr
    wdt <- wdt[,admnum:=cumsum(new.adm),by=pnr]
    setkey(wdt,pnr,admnum)
    wdt <- wdt[,':='(first.inddto=inddto[1],last.uddto=uddto[.N]),by=list(pnr,admnum)]
    ## remove temporary variables
    wdt <- wdt[,c("new.adm","p.inddto","p.uddto","max.uddto","p.max.uddto"):=NULL]
    wdt
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here
