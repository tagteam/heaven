#' @title Comorbidity with drugs
#'
#' @param data Raw data with atc codes, eksd needs specific variables: pnr, atc, eksd
#' @param pnr Variable with ID for each subject/group. Default name: pnr 
#' @param atc Variable with atc codes. Must be type character. Default name: atc 
#' @param eksd Variable with dates. Must be type Date or numeric. Default name: eksd
#' @param code Vector with ATC codes for the drugs where the first-instance-date is desired
#' @param date Date of end of search "YYYY-MM-DD"
#' @param interval Length of the interval to search (days before the argument 'date')
#'
#' @return The function returns the date for the first drug with any of the ATC codes in a 
#' data.table with pnr, ATC and first date. If a date and interval are given, the first date in 
#' the interval before the date are returned.
#' @export
#'
#' @examples
#' library(data.table)
#' dat<-data.table(pnr=1:22, 
#'                 atc=rep(c("A01AB04", "A03AA04", "N02BE01", "M01AE01", "R03CC02", "S01XA20"), each=5), 
#'                 eksd=c("2001-01-18","2002-02-20","2002-01-10","2001-09-10","1999-09-10"))
#' dat[,eksd:=as.Date(eksd)]
#'                
#' #Find patients who have received drugs with atc codes starting with 'S01' or 'R03' and report the date of the first occurrence for each patient
#' comorbidityDrugs(dat, code=c('S01', 'R03'))
#' 
#' #Find patients who have received drugs with atc codes starting with 'N' or 'M' in the period from "2001-03-01" to "2002-03-01" and report the date of the first occurrence
#' comorbidityDrugs(dat, code=c('N', 'M'), date="2002-03-21", interval=365)
#' 
#' #With custom names
#' dat2<-data.table(MyID=1:22, 
#'                  MyAtcCodes=rep(c("A01AB04", "A03AA04", "N02BE01", "M01AE01", "R03CC02", "S01XA20"), each=5), 
#'                  MyDates=c("2001-01-18","2002-02-20","2002-01-10","2001-09-10","1999-09-10"))
#' dat2[,MyDates:=as.Date(MyDates)]
#' comorbidityDrugs(dat2, pnr='MyID', atc='MyAtcCodes', eksd='MyDates', code=c('S01', 'R03'))
#' 
#' @author Helle Hoejmark Eriksen <helle.e@@rn.dk>

comorbidityDrugs<- function(data=NULL,pnr='pnr',atc='atc',eksd='eksd',code=NULL,date=NULL,interval=NULL){
  ## Check of input  
  options(warn=1)
  if(is.null(data)){
    warning("Argument 'data' is missing")
    return(NA)
  }
  if(is.null(code)){
    warning("Argument 'code' is missing")
    return(NA)
  }
  if(!is.vector(code)){
    warning("Argument 'code' is not a vector")
    return(NA)
  }
  if(!is.character(code[1])){  
    warning("Argument 'code' is not a vector of strings") 
    return(NA)
  }
  if(!is.null(date)&&is.na(as.Date(date))){ 
    warning("Argument 'date' is not a date") 
    return(NA)
  }
  if(!is.null(interval)&&!is.numeric(interval)){ 
    warning("Argument 'interval' is not numeric")
    return(NA)
  }
  if(!is.null(interval)&&is.null(date)){ 
    warning("Argument 'date' is missing")
    return(NA)
  }
  if(!is.null(date)&&is.null(interval)){
    warning("Argument 'interval' is missing")
    return(NA)
  }
  
  ##  Make into data.table and change all variables to lower case
  out <- as.data.table(data)
  
  ## A different name for the variables: xTempName
  setnames(out,pnr,'pnrxTempName')
  setnames(out,atc,'atcxTempName')
  setnames(out,eksd,'eksdxTempName')
  
  ## Check variable types 
  if( !( class(out[,eksdxTempName])=="Date" | is.numeric(out[,eksdxTempName]) ) ){stop("eksd must be numeric or Date")} 
  if( !is.character(out[,atcxTempName]) ){stop("atc must be character")} #else grep won't work
  
  ## Removes Invalid dates
  #eksd
  outerrorTemp <- copy(out[is.na(eksdxTempName)])
  out <- out[!is.na(eksdxTempName)]
  if( dim(outerrorTemp)[1]!=0 ){message("Some eksd are missing and have been removed")}
  #atc
  outerror=rbind(outerrorTemp, out[atcxTempName=='',] ) #should we output this also?
  out <- out[atcxTempName!='']
  if( dim(outerror)[1]!=dim(outerrorTemp)[1] ){message("Some atc are missing and have been removed")}
  
  
  ## Find the unique diagnosis and the patients with them
  setkey(out,atcxTempName) #Sort by key
  diag <- out[.(unique(atcxTempName)),,mult="first"] #Unique diagnosis
  diag <- diag[unlist(lapply(paste('^',code,sep=''),grep,atcxTempName))] #The unique diagnosis of interest
  out <- out[.(diag$atcxTempName),nomatch = 0L] #Patients with unique diagnosis of interest
  
  if(!is.null(date)&!is.null(interval)){  
    #Restrict diagnoses to specific period in time
    date<-as.integer(as.Date(date)) #Can't convert directly to integer - Speed up                                                                                              
    out <- out[date-interval<=eksdxTempName & eksdxTempName<date]
  }
  
  ## Take first diagnosis if more than one for each patient
  setkey(out,pnrxTempName,eksdxTempName) #Order data by pnr and increasing eksd
  out <- out[.(unique(pnrxTempName)),,mult="first"] 
  
  ## Change names back
  setnames(out,'pnrxTempName',pnr)
  setnames(out,'atcxTempName',atc)
  setnames(out,'eksdxTempName',eksd)
  
  return(out) 
}