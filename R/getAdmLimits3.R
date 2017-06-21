##' Get Admission time
##'
##' Removes row errors and returns data with add admission time
##' @param dt data.table
##' @param personid Variable with ID for each subject/group
##' @param inddto Variable with entry times. Must be date or integer
##' @param uddto Variable with exit times. Must be date or integer
##' @return list containing dataset with admission dates add to data and dataset containing row errors
##' @export 
##' @examples
##' newpop <- getAdmLimits3(mydata,'Pnr','Entrydate','Outdate')


getAdmLimits3 <- function(dt,personid,inddto,uddto){
  # Data must be a data.table
  if (!is.data.table(dt)) stop("Error: Data is not data.table")
  # Makes hard copy
  wdt2=copy(dt)
  # A different name for the variables: XoXoXo
  setnames(wdt2,uddto,'uddtoXoXoXo')
  setnames(wdt2,inddto,'inddtoXoXoXo')
  setnames(wdt2,personid,'personidXoXoXo')
  ## Copy rows with NA in inddto or uddto, into dataframe wdterror and change names back
  wdterror=copy(wdt2[is.na(inddtoXoXoXo)|is.na(uddtoXoXoXo)|inddtoXoXoXo>uddtoXoXoXo])
  setnames(wdterror,'uddtoXoXoXo',uddto)
  setnames(wdterror,'inddtoXoXoXo',inddto)
  setnames(wdterror,'personidXoXoXo',personid)
  ## Removes rows with inddto after uddto and NA in inddto or/and uddto
  wdt2 <- wdt2[inddtoXoXoXo<=uddtoXoXoXo]
  ##Converts Date to integer
  if ((is.integer(wdt2[,inddtoXoXoXo]))&(is.integer(wdt2[,uddtoXoXoXo]))){
    datechecker=FALSE
  }else{
    wdt2[,inddtoXoXoXo:=as.integer(inddtoXoXoXo)]
    wdt2[,uddtoXoXoXo:=as.integer(uddtoXoXoXo)]
    datechecker=TRUE
  }
  ## Sort by personid and admission dates
  setkey(wdt2,personidXoXoXo,inddtoXoXoXo)
  ## Latest admission date by personid
  wdt2[,`:=`(max.outdate=cummax(uddtoXoXoXo)),by=personidXoXoXo] 
  ## Start of each admission (1L - the L forces R to store it as an integer)
  wdt2[,startadm:=1L*(inddtoXoXoXo>shift(max.outdate)),by=personidXoXoXo]
  wdt2[.(unique(personidXoXoXo)),startadm:=1L,mult="first"]
  ## overlapping admissions
  wdt2[,startadm:=cumsum(startadm),by=personidXoXoXo]
  ## Start and end date for the hospitalization the admission is part of.
  wdt2[,':='(startadm=inddtoXoXoXo[1] , max.outdate=max.outdate[.N] ) ,by=.(personidXoXoXo,startadm)]
  ## Change names to what the two variables now contain
  setnames(wdt2,c("startadm","max.outdate"),c("first.inddto","last.outdate"))
  ## Convert if input were format Date
  if(datechecker==TRUE){
    wdt2[,uddtoXoXoXo:=as.Date(uddtoXoXoXo,origin="1970-01-01")]
    wdt2[,inddtoXoXoXo:=as.Date(inddtoXoXoXo,origin="1970-01-01")]
    wdt2[,last.outdate:=as.Date(last.outdate,origin="1970-01-01")]
    wdt2[,first.inddto:=as.Date(first.inddto,origin="1970-01-01")]
  }
  #change names back
  setnames(wdt2,'uddtoXoXoXo',uddto)
  setnames(wdt2,'inddtoXoXoXo',inddto)
  setnames(wdt2,'personidXoXoXo',personid)
  
  codehell <- list("TheData"=wdt2,"RowErrors" =wdterror)   
  return(codehell)
}







