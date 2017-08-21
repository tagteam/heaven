#' @title Comorbidity with drugs
#'
#' @param data Raw data with atc codes, needs specific variables: pnr, atc, eksd 
#' @param code Vector with ATC codes for the drugs where the first-instance-date is desired
#' @param date Date of end of search
#' @param interval Length of the interval to search (days before the argument 'date')
#'
#' @return The function returns the date for the first drug with any of the ATC codes in a 
#' data.table with pnr, ATC and first date. If a date and interval are given, the first date in 
#' the interval before the date are returned.
#' @export
#'
#' @examples
#' library(data.table)
#' dat<-data.table(pnr=1:22, atc=rep(c("A01AB04", "A03AA04", "N02BE01", "M01AE01", "R03CC02", "S01XA20"),each=5), eksd=c("2001-01-18","2002-02-20","2002-01-10","2001-09-10","1999-09-10"))
#' #Find patients who have received drugs with atc codes starting with 'S01' or 'R03' and report the date of the first occurrence for each patient
#' comorbidityDrugs(dat, code=c('S01', 'R03'))
#' #Find patients who have received drugs with atc codes starting with 'N' or 'M' in the period from "2001-03-01" to "2002-03-01" and report the date of the first occurrence
#' comorbidityDrugs(dat, code=c('N', 'M'), date="2002-03-01", interval=365)
#' @author Helle Hoejmark Eriksen <helle.e@@rn.dk>
comorbidityDrugs<- function(data=NULL,code=NULL,date=NULL,interval=NULL){
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
  d <- as.data.table(data)
  var.names <- tolower(colnames(d)) 
  colnames(d) <- var.names 
  
  ## Find the unique diagnosis and the patients with them
  setkey(d,atc) #Sort by key
  out <- d[.(unique(atc)),,mult="first"] #unique diagnosis
  out <- out[unlist(lapply(paste('^',code,sep=''),grep,atc))] #the unique diagnosis of interest
  out <- d[.(out$atc),nomatch = 0L] #Patients with unique diagnosis of interest
  
  if(!is.null(date)&!is.null(interval)){
    #Restrict diagnoses to specific period in time
    date<-as.Date(date)
    out <- out[date-interval<=eksd&eksd<date]
  }
  
  ## Take first diagnosis if more than one for each patient
  setkey(out,pnr,eksd) #Order data by pnr and increasing eksd
  out <- out[.(unique(pnr)),,mult="first"] 
  
  ## Output to return
  out <- out[,.(pnr,atc,eksd)]
  
  return(list(data=out,atc=code,date=date, interval=interval))
}