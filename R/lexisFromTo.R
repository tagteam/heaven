#' @title lexisFromTo
#' @description 
#' lexixFromTo is a specialised version of lexis splitting.  In handling drug treatment, employment
#' and similar factors the individual may move in and out of conditions.  This function splits 
#' according such a situation.  The input is first "base data" id/in/out/event (and other variables) that
#' may already have been split by other functions.  The other data is a sequence of 
#' id/from/to/value/name - that may represent multiple conditions with from/to and with "name" to
#' distinguish.
#' @usage
#' lexisFromTo(indat,splitdat,invars,splitvars,default="0")
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end, event and other data - possibly 
#' already split
#' @param splitdat - Data with splitting guide - id/from/to/value/name 
#' @param invars - vector of colum names for id/entry/exit/event - in that 
#' order, 
#' example: c("id","start","end","event")
#' @param splitvars - vector of column names containing dates to split by.
#' example: c("id","start","end","value","name") - must be in that order!
#' @param default - Value given to intervels not given a value by the function.
#' @return
#' The function returns a new data table where records have been split according 
#' to the splittingguide dataset. Variables
#' unrelated to the splitting are left unchanged.
#' @details 
#' The input to this function are two data.tables and two lists of the critical 
#' variables.  The BASE data it the data to be split.
#' This data must have a variable to identify participants, start/end times and 
#' a variable to indicate event after last interval.
#' The other table (SPLITTINGUIDE) contains possibly multiple records for each 
#' participants with id/from/to/value/name.
#' The program checks that intervals are not negative and that intervals for 
#' each "name" and each individual do not overlap.  Violation stops the program 
#' with error. Overlaps may occur in real situations, but the user needs to make 
#' decisions regarding this prior to this function.
#' 
#' It is required that the splittingguide contains at least one record.  
#' Missing data for key variables are not allowed and will cause errors.
#' @examples
#' library(data.table)
#' dat <- data.table(id=c("A","A","B","B","C","D"),
#'                  start=as.Date(c(0,100,0,100,200,400),origin='1970-01-01'),
#'                  end=as.Date(c(100,200,100,200,300,500),origin='1970-01-01'),
#'                  event=c(0,1,0,0,1,1))
#' split <- data.table (id=c("A","A","A","A","B","B","B","D","D"),
#'                     start=as.Date(c(0,50,25,150,110,150,400,300,500),origin='1970-01-01'),
#'                     end= as.Date(c(25,75,150,151,120,250,500,400,500),origin='1970-01-01'),
#'                     value=c(1,2,3,4,1,2,3,6,7),
#'                     name=c("d1","d1","d2","d2","d1","d1","d2","d3","d4"))
#' #Show the dataset:
#' dat[]
#' split[]                   
#' temp <- lexisFromTo(dat # inddato with id/in/out/event
#'                    ,split # Data with id and dates
#'                    ,c("id","start","end","event") #names of id/in/out/event - in that order
#'                   ,c("id","start","end","value","name")) #Nmes var date-vars to split by
#' @export
lexisFromTo <- function(indat # inddato with id/in/out/event - and possibly other variables
                       ,splitdat # Data with from/to/Value
                       ,invars #names of id/in/out/event - in that order
                       ,splitvars #Names in splitdat with pnr/from/to/value/name
                       ,default="0"
                        ){
    .N=pnr=pnrnum=.GRP=mergevar2=start=slut=.SD=dif1=prior_slut=mergevar=inn=name=val=event=out=num=isdate=NULL
    setDT(indat)
    setDT(splitdat)
    #Tests of data
    INDAT <- indat[,invars,with=FALSE] # Necessary variables for split
    INDAT[,mergevar:=1:.N] # Variable to merge by after split;
    setnames(INDAT,invars,c("pnr","inn","out","event"))
    if (lubridate::is.Date(INDAT[,inn])){
      isdate <- TRUE
      INDAT[,':='(inn=as.numeric(inn),out=as.numeric(out))] 
    }
    else isdate <- FALSE
    if (!class(INDAT[,event]) %in% c("numeric","integer")) stop("event variable must be integer - zero or one") 
    setkey(INDAT,pnr)
    INDAT[,pnrnum:=.GRP,by="pnr"] # Number pnr - As a consecutive sequence
    pnrgrp <-unique(INDAT[,c("pnr","pnrnum"),with=FALSE]) 
    RESTDAT <- copy(indat)[,(invars[2:4]):=NULL]# Other variables to be added at end
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
    temp <- csplit[start>slut,sum(start>slut)]
    if (temp>0) stop("Error - Attempt to split with negative date intervals in splitting guide") 
    precols = paste("prior", c("start","slut"), sep="_")
    cols <-c("start","slut")
    csplit[, (precols) := shift(.SD, 1, 0, "lag"), .SDcols=cols,by=c("name","pnrnum")]
    error <-dim(csplit[start-prior_slut<0,])[1]
    if (error>0) stop("Error in splitting guide data - Intervals overlapping - unpredictable results")    
    suppressWarnings(csplit[,(precols):=NULL])#remove extra variables
    # Get list of names
    nams <- unique(csplit[["name"]]) # provides order of names for later renaming
    name <- data.table(nams,1:length(nams))
    setnames(name,c("name","num"))
    csplit <- merge(csplit,name,by="name")
    csplit[,name:=NULL]
    setcolorder(csplit,c("pnrnum","start","slut","num"))
    csplit[,':='(start=as.integer(start),slut=as.integer(slut))] # integers need for change to matrix
    setkeyv(csplit,c("pnrnum","start","slut","num")) # sorted with increasing dates
    OUT <- .Call('_heaven_splitFT',PACKAGE = 'heaven',
                INDAT[,pnrnum], # PNR as sequence number - base data
                INDAT[,inn], # Starttimes - base data
                INDAT[,out], # Endtimes - base data
                INDAT[,event], # Event at end of interval 0/1 - base data
                INDAT[,mergevar], # Merge variable, multiple records can have same pnr - base data
                csplit[,pnrnum], # Sequence number of pnr in split guide
                csplit[,val], # Value of name to provide to output for interval - split guide
                csplit[,start], # Interval start - split guide
                csplit[,slut], # Interval end - split guide
                csplit[,num], #Covariate number
                length(nams),
                default) # Number of covariate to split by) # Call c++
    # OUT<- splitFT(INDAT[,pnrnum], # PNR as sequence number - base data
    #               INDAT[,inn], # Starttimes - base data
    #               INDAT[,out], # Endtimes - base data
    #               INDAT[,event], # Event at end of interval 0/1 - base data
    #               INDAT[,mergevar], # Merge variable, multiple records can have same pnr - base data
    #               csplit[,pnrnum], # Sequence number of pnr in split guide
    #               csplit[,val], # Value of name to provide to output for interval - split guide
    #               csplit[,start], # Interval start - split guide
    #               csplit[,slut], # Interval end - split guide
    #               csplit[,num], #Covariate number
    #               length(nams), # Number of covariate to split by
    #               default)
    OUT1 <- cbind(setDT(OUT[1:5]))
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
    setnames(OUT,c("pnr","inn","out","event"),invars) 
    OUT[,c("mergevar","pnrnum"):=NULL]
  OUT[]
}
