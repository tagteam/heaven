### getAdmLimits.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43)
## Version:
## last-updated: Sep  20 2017 (12:45) 
##           By: Peter Enemark Lund
##     Update #: 19
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Adds first admission date and last discharge date to a sequence of admissions. The default input data.table is the current standard
##' by which admissions are available in Statistics Denmark. 
##'
##' @title Get Admission time
##' @param dt data.table containing the variables
##' @param pnr Variable with ID for each subject/group (default is \code{pnr}).
##' @param inddto Variable with entry times. Must be date or numeric (default is \code{inddto}). 
##' @param uddto Variable with exit times. Must be date or numeric (default is \code{uddto}). 
##' @return List containing two datasets. 
##'         First contains the same data.table as for input but with 2 new variables first.inddto and last.outdate.
##'         Second contains all rows with missing data or where inddto > uddto.
##' @examples
##' \dontrun{
##' pop <- as.data.table(data(sample.diag))
##' ## Varaibles have default names
##' newpop <- getAdmLimits(pop)
##' 
##' ## Varaibles have custom names
##' setnames(pop, c('pnr','inddto','uddto'), c('personid', 'Entrydate','Outdate'))
##' newpop <- getAdmLimits(pop,pnr='personid',inddto='Entrydate',uddto='Outdate')
##' 
##' ## List is returned containing two data.tables
##' newpopData <- newpop[[1]]
##' newpopError <- newpop[[2]]
##' }
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
getAdmLimits <- function(dt,
                         pnr="pnr",
                         inddto="inddto",
                         uddto="uddto"){
inddtoxTempName <- uddtoxTempName <- pnrxTempName <- startadm <- max.outdate <- .N <- first.inddto <- last.outdate <- NULL
  ## Data must be a data.table
  if (!is.data.table(dt)){
    stop("Error: data is not type data.table")
  }
  ## Makes hard copy
  wdt=copy(dt)
  ## A different name for the variables: xTempName
  setnames(wdt,uddto,'uddtoxTempName')
  setnames(wdt,inddto,'inddtoxTempName')
  setnames(wdt,pnr,'pnrxTempName')
  ## Check variable types
  datecheckerInd=FALSE
  if (class(wdt[,inddtoxTempName])=="Date"){
    datecheckerInd=TRUE
  } else if (!is.numeric(wdt[,inddtoxTempName])){
    stop("Error: inddto must be numeric or Date")
  }  
  datecheckerUd=FALSE
  if (class(wdt[,uddtoxTempName])=="Date"){
    datecheckerUd=TRUE
  } else if (!is.numeric(wdt[,uddtoxTempName])){
    stop("Error: uddto must be numeric or Date")
  }    
  ## Copy rows with inddto>uddto and NA in inddto/uddto, into data.table wdterror and change names back
  wdterror=copy(wdt[is.na(inddtoxTempName)|is.na(uddtoxTempName)|inddtoxTempName>uddtoxTempName])
  setnames(wdterror,'uddtoxTempName',uddto)
  setnames(wdterror,'inddtoxTempName',inddto)
  setnames(wdterror,'pnrxTempName',pnr)
  ## Removes rows copied into wdterror
  wdt <- wdt[inddtoxTempName<=uddtoxTempName]
  ## Convert Dates to numeric
  if (datecheckerInd==TRUE){
    wdt[,inddtoxTempName:=as.integer(inddtoxTempName)]  
  }
  if (datecheckerUd==TRUE){
    wdt[,uddtoxTempName:=as.integer(uddtoxTempName)] 
  }
  ## Sort by pnr and admission dates
  setkey(wdt,pnrxTempName,inddtoxTempName)
  ## Latest admission date by pnr
  wdt[,`:=`(max.outdate=cummax(uddtoxTempName)),by=pnrxTempName] 
  ## Start of each admission (1L - the L forces R to store it as an integer)
  wdt[,startadm:= 1L*(inddtoxTempName>shift(max.outdate)),by=pnrxTempName] 
  wdt[.(unique(pnrxTempName)),startadm:=1L,mult="first"]
  ## Overlapping admissions
  wdt[,startadm:=cumsum(startadm),by=pnrxTempName]
  ## Start and end date for the hospitalization the admission is part of.
  wdt[,':='(startadm=inddtoxTempName[1] , max.outdate=max.outdate[.N] ) ,by=.(pnrxTempName,startadm)]
  ## Change names to what the two variables now contain
  setnames(wdt,c("startadm","max.outdate"),c("first.inddto","last.outdate"))
  ## Change columns around
  refcols <- c("first.inddto","last.outdate")
  setcolorder(wdt, c(setdiff(names(wdt),refcols),refcols))
  ## Convert if input were format Date
  if(datecheckerInd==TRUE){
    wdt[,inddtoxTempName:=as.Date(inddtoxTempName,origin="1970-01-01")]
  }
  if(datecheckerUd==TRUE){
    wdt[,uddtoxTempName:=as.Date(uddtoxTempName,origin="1970-01-01")]
  }
  wdt[,first.inddto:=as.Date(first.inddto,origin="1970-01-01")]
  wdt[,last.outdate:=as.Date(last.outdate,origin="1970-01-01")]
  ## Change names back
  setnames(wdt,'uddtoxTempName',uddto)
  setnames(wdt,'inddtoxTempName',inddto)
  setnames(wdt,'pnrxTempName',pnr)
  ## Create list and return
  wdtList <- list("AdmLimits"=wdt,"RowErrors"=wdterror)   
  wdtList
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here