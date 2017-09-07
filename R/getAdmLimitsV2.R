##' Removes row errors and returns data with add admission time
##' 
##' @title Get Admission time
##' @param dt data.table containing the variables
##' @param personid Variable with ID for each subject/group. Default name pnr
##' @param inddto Variable with entry times. Must be date or numeric. Default name inddto
##' @param uddto Variable with exit times. Must be date or numeric. Default name uddto
##' @return List containing dataset with admission dates add to data and dataset containing row errors
##' @export 
##' @examples
##' \dontrun{
##' newpop <- getAdmLimits3(mydata,'Pnr','Entrydate','Outdate')
##' 
##' ## List is returned containing two data.tables
##' newpopData <- newpop[[1]]
##' newpopError <- newpop[[2]]
##' }
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>, Peter Enemark Lund <peter.l@rn.dk>

getAdmLimitsV2 <- function(dt,personid='pnr',inddto='inddto',uddto='uddto'){
  # Data must be a data.table
  if (!is.data.table(dt)) stop("Error: Data is not data.table")
  # Makes hard copy
  wdt=copy(dt)
  # A different name for the variables: xTempName
  setnames(wdt,uddto,'uddtoxTempName')
  setnames(wdt,inddto,'inddtoxTempName')
  setnames(wdt,personid,'personidxTempName')
  # Check variable types
  datecheckerInd=FALSE
  if (class(wdt[,inddtoxTempName])=="Date"){
    datecheckerInd=TRUE
  } else if (!is.numeric(wdt[,inddtoxTempName])){stop("Error: inddto must be numeric or Date")}  
  datecheckerUd=FALSE
  if (class(wdt[,uddtoxTempName])=="Date"){
    datecheckerUd=TRUE
  } else if (!is.numeric(wdt[,uddtoxTempName])){stop("Error: uddto must be numeric or Date")}    
  ## Copy rows with NA in inddto or uddto, into dataframe wdterror and change names back
  wdterror=copy(wdt[is.na(inddtoxTempName)|is.na(uddtoxTempName)|inddtoxTempName>uddtoxTempName])
  setnames(wdterror,'uddtoxTempName',uddto)
  setnames(wdterror,'inddtoxTempName',inddto)
  setnames(wdterror,'personidxTempName',personid)
  ## Removes rows with inddto after uddto and NA in inddto or/and uddto
  wdt <- wdt[inddtoxTempName<=uddtoxTempName]
  ##Converts Date to numeric
  if (datecheckerInd==TRUE){
    wdt[,inddtoxTempName:=as.integer(inddtoxTempName)]  
  }
  if (datecheckerUd==TRUE){
    wdt[,uddtoxTempName:=as.integer(uddtoxTempName)] 
  }
  ## Sort by personid and admission dates
  setkey(wdt,personidxTempName,inddtoxTempName)
  ## Latest admission date by personid
  wdt[,`:=`(max.outdate=cummax(uddtoxTempName)),by=personidxTempName] 
  ## Start of each admission (1L - the L forces R to store it as an integer)
  wdt[,startadm:= 1L*(inddtoxTempName>shift(max.outdate)),by=personidxTempName] 
  wdt[.(unique(personidxTempName)),startadm:=1L,mult="first"]
  ## overlapping admissions
  wdt[,startadm:=cumsum(startadm),by=personidxTempName]
  ## Start and end date for the hospitalization the admission is part of.
  wdt[,':='(startadm=inddtoxTempName[1] , max.outdate=max.outdate[.N] ) ,by=.(personidxTempName,startadm)]
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
  #change names back
  setnames(wdt,'uddtoxTempName',uddto)
  setnames(wdt,'inddtoxTempName',inddto)
  setnames(wdt,'personidxTempName',personid)
  # Create list and return
  wdtList <- list("TheData"=wdt,"RowErrors" =wdterror)   
  return(wdtList)
}
