#' @title splitFromTo
#' @description 
#' splitFromTo is devised to split a series of records in multiple records based
#' on entry/exit dates provided from another splitting guide dataset. The function
#' can handle multiple different events with each type of event characterized by
#' the variable "value".
#' 
#' The function is useful for analysis of data where temporary changes occur.
#' These may be drugs treatment, pregnancy etc.  For a situation of pregnancy
#' for example it may be useful for analysis to split a person into three 
#' records representing time before, during and after pregnancy.
#' 
#' The input is first "base data" id/in/out (and other variables) that
#' may already have been split by other functions.  The other data is a sequence 
#' of id/from/to/value/name - that may represent multiple conditions with 
#' from/to and with "name" to distinguish.
#' 
#' When no value is provided by the splitting guide the "default" is used. If
#' this is not provided the value is set to "0" 
#' 
#' @usage
#' splitFromTo(indat,splitdat,invars,splitvars,default="0",datacheck=TRUE)
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end and other data - possibly 
#' already split
#' @param splitdat - Data with splitting guide - id/from/to/value/name 
#' @param invars - vector of column names for id/entry/exit - in that 
#' order, 
#' example: c("id","start","end")
#' @param splitvars - vector of column names containing dates to split by.
#' example: c("id","start","end","value","name") - must be in that order!
#' @param default - Value given to intervals not given a value by the function.
#' @param datacheck - This function would crash or produce incorrect results if
#' input data have overlapping intervals or negative intervals in any of the two
#' input datasets.  Ts is checked and error produced by datacheck. Can be 
#' set to FALSE if data are checked otherwise
#' @return
#' The function returns a new data table where records have been split according 
#' to the splitting guide dataset. Variables unrelated to the splitting are 
#' left unchanged.
#' @details 
#' The input to this function are two data.tables and two lists of the critical 
#' variables.  The BASE data it the data to be split.
#' This data must have a variable to identify participants and start/end times. 
#'
#' The other table (SPLITTINGUIDE) contains possibly multiple records for each 
#' participants with id/from/to/value/name - or just id/from/to
#' 
#' The program checks that intervals are not negative and that intervals for 
#' each "name" and each individual do not overlap.  Violation stops the program 
#' with error. Overlaps may occur in real situations, but the user needs to make 
#' decisions regarding this prior to using this function.
#' 
#' It is required that the splitting guide contains at least one record.  
#' Missing data for key variables are not allowed and will cause errors.
#' 
#' @examples
#' library(data.table)
#' dat <- data.table(id=c("A","A","B","B","C","D"),
#'                  start=as.Date(c(0,100,0,100,200,400),origin='1970-01-01'),
#'                  end=as.Date(c(100,200,100,200,300,500),origin='1970-01-01'))
#' split <- data.table (id=c("A","A","A","A","B","B","B","D","D"),
#'                     start=as.Date(c(0,50,25,150,110,150,400,300,500),origin='1970-01-01'),
#'                     end= as.Date(c(25,75,150,151,120,250,500,400,500),origin='1970-01-01'),
#'                     value=c(1,2,3,4,1,2,3,6,7),
#'                     name=c("d1","d1","d2","d2","d1","d1","d2","d3","d4"))
#' #Show the dataset:
#' dat[]
#' split[]                   
#' temp <- splitFromTo(dat # inddato with id/in/out/event
#'                    ,split # Data with id and dates
#'                    ,c("id","start","end") #names of id/in/out/event - in that order
#'                   ,c("id","start","end","value","name")) #Names var date-vars to split by
#' temp[]                   
#' @export
splitFromTo <- function(indat # inddato with id/in/out/event - and possibly other variables
                       ,splitdat # Data with from/to/Value
                       ,invars #names of id/in/out/event - in that order
                       ,splitvars #Names in splitdat with pnr/from/to/value/name
                       ,default="0"
                       ,datacheck=TRUE
                        ){

  .N=pnr=pnrnum=.GRP=mergevar2=start=slut=.SD=dif1=prior_slut=mergevar=inn=name=val=out=num=isdate=NULL
  indat <- setDT(copy(indat))
  splitdat <- setDT(copy(splitdat))
  #Tests of data
  INDAT <- indat[,invars,with=FALSE] # Necessary variables for split
  INDAT[,mergevar:=1:.N] # Variable to merge by after split;
  setnames(INDAT,invars,c("pnr","inn","out"))
  if (lubridate::is.Date(INDAT[,inn])){
    isdate <- TRUE
    INDAT[,':='(inn=as.numeric(inn),out=as.numeric(out))] 
  }
  else isdate <- FALSE
  if(datacheck){
    temp <- INDAT[,list(num=sum(out<inn))]
    if (temp[,num]>0) stop("Error - end of intervald cannot come before start of intervals")
    if (!class(INDAT[,inn]) %in% c("numeric","integer","Date") | !class(INDAT[,out]) %in% c("numeric","integer","Date")) 
      stop("input date not Date or integer or numeric")
    setkeyv(INDAT,"inn")
    temp <- INDAT[,list(num=sum(inn<shift(out,fill=inn[1]))),by="pnr"]
    temp <- temp[,list(num=sum(num))]
    if(temp[,num]>0) stop('Error - Data "indat" includes overlapping intervals')
  }    
  setkey(INDAT,pnr)
  INDAT[,pnrnum:=.GRP,by="pnr"] # Number pnr - As a consecutive sequence
  pnrgrp <-unique(INDAT[,c("pnr","pnrnum"),with=FALSE]) 
  RESTDAT <- copy(indat)[,(invars[2:3]):=NULL]# Other variables to be added at end
  RESTDAT[,mergevar:=1:.N]  # Merge after split assuming possible prior splits 
  setnames(RESTDAT,invars[1],"pnr")
  #Prepare splitdat
  csplit <- copy(splitdat[,splitvars,with=FALSE]) # necessary variables
  setnames(csplit,splitvars,c("pnr","start","slut","val","name"))
  csplit[,val:=as.character(val)] # Character necessary for c-program 
  setkey(csplit,"pnr")
  csplit <- merge(csplit,pnrgrp,by="pnr")
  csplit[,pnr:=NULL] # identify only by pnrnum
  setkeyv(csplit,c("pnrnum","start"))
  # Check csplit content
  if (datacheck){
    temp <- csplit[start>slut,sum(start>slut)]
    if (temp>0) stop("Error - Attempt to split with negative date intervals in splitting guide") 
    csplit[,prior_slut:=shift(slut),by=c("name","pnrnum")]
    error <-dim(csplit[!is.na(prior_slut) & start-prior_slut<0,])[1]
    if (error>0) stop("Error in splitting guide data - Intervals overlapping - unpredictable results")    
    csplit[,prior_slut:=NULL]
  }
  # Get list of names
  nams <- unique(csplit[["name"]]) # provides order of names for later renaming
  name <- data.table(nams,1:length(nams))
  setnames(name,c("name","num"))
  csplit <- merge(csplit,name,by="name")
  csplit[,name:=NULL]
  setcolorder(csplit,c("pnrnum","start","slut","num"))
  csplit[,':='(start=as.integer(start),slut=as.integer(slut))] # integers need for change to matrix
  setkeyv(csplit,c("pnrnum","start","slut","num")) # sorted with increasing dates
  # OUT <- .Call('_heaven_splitft',PACKAGE = 'rtmle',
  #              INDAT[,pnrnum], # PNR as sequence number - base data
  #              INDAT[,inn], # Starttimes - base data
  #              INDAT[,out], # Endtimes - base data
  #              INDAT[,event] # Dummy
  #              INDAT[,mergevar], # Merge variable, multiple records can have same pnr - base data
  #              csplit[,pnrnum], # Sequence number of pnr in split guide
  #              csplit[,val], # Value of name to provide to output for interval - split guide
  #              csplit[,start], # Interval start - split guide
  #              csplit[,slut], # Interval end - split guide
  #              csplit[,num], #Covariate number
  #              length(nams),
  #              default) # Number of covariate to split by) # Call c++
  INDAT[,event:=0] # Dummy to fit c++ function
  OUT<- splitft(INDAT[,pnrnum], # PNR as sequence number - base data
                INDAT[,inn], # Starttimes - base data
                INDAT[,out], # Endtimes - base data
                INDAT[,event], # Dummy
                INDAT[,mergevar], # Merge variable, multiple records can have same pnr - base data
                csplit[,pnrnum], # Sequence number of pnr in split guide
                csplit[,val], # Value of name to provide to output for interval - split guide
                csplit[,start], # Interval start - split guide
                csplit[,slut], # Interval end - split guide
                csplit[,num], #Covariate number
                length(nams), # Number of covariate to split by
                default)
  OUT1 <- cbind(setDT(OUT[1:5]))
  OUT1[,event:=NULL]
  OUT2 <- setDT(do.call(cbind,OUT[6]))
  setnames(OUT2,nams)
  OUT <- cbind(OUT1,OUT2)
  
  if(isdate){
    OUT[,':='(inn=as.Date(inn,origin="1970-01-01"),out=as.Date(out,origin="1970-01-01"))]
  }
  setkey(OUT, mergevar, inn)
  setkey(RESTDAT, mergevar)
  OUT <- merge(OUT,RESTDAT, by=c("mergevar"),all=TRUE)
  setkeyv(OUT,c("pnr","inn","out"))
  setnames(OUT,c("pnr","inn","out"),invars) 
  OUT[,c("mergevar","pnrnum"):=NULL]
  OUT[]
}

