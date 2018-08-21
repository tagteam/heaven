#' @title lexisFromTo
#' @description 
#' lexixFromTo is a specialised version of lexis splitting.  In handling drug treatment, employment
#' and similar factors the individual may move in and out of conditions.  This function splits 
#' according such a situation.  The input is first "base data" id/in/out/event (and other variables) that
#' may already have been split by other functions.  The other data is a sequence of 
#' id/from/to/value/name - that may represent multiple conditions with from/to and with "name" to
#' distinguish.
#' @usage
#' lexisFromTo(indat,splitdat,invars,splitvars)
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end, event and other data - possibly 
#' already split
#' @param splitdat - Data with splitting guide - id/from/to/value/name 
#' @param invars - vector of colum names for id/entry/exit/event - in that 
#' order, 
#' example: c("id","start","end","event")
#' @param splitvars - vector of column names containing dates to split by.
#' example: c("id","start","end","value","name") - must be in that order!
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
#' dat <- data.table(id=c("A","A","B","B"),
#'                  start=c(0,100,0,100),
#'                  end=c(100,200,100,200),
#'                  event=c(0,1,0,0))
#' split <- data.table (id=c("A","A","A","B","B","B"),
#'                     start=c(0,50,25,110,150,400),
#'                     end= c(25,75,150,120,250,500),
#'                     value=c(1,2,3,1,2,3),
#'                     name=c("d1","d1","d2","d1","d1","d2"))
#' temp <- lexisFromTo(dat # inddato with id/in/out/event
#'                    ,split # Data with id and dates
#'                    ,c("id","start","end","event") #names of id/in/out/event - in that order
#'                   ,c("id","start","end","value","name")) #Nmes var date-vars to split by
#' temp[]
#' @export
lexisFromTo <- function(indat # inddato with id/in/out/event - and possibly other variables
                       ,splitdat # Data with from/to/Value
                       ,invars #names of id/in/out/event - in that order
                       ,splitvars #Names in splitdat with pnr/from/to/value/name
                        ){
    .N=pnr=pnrnum=.GRP=mergevar2=start=slut=.SD=dif1=prior_slut=mergevar=inn=name=NULL
    copyindat <- copy(indat)
    setDT(copyindat)
    splitdat <- copy(splitdat)
    setDT(splitdat)
    #Tests of data
    INDAT <- copyindat[,invars,with=FALSE] # Necessary variables for split
    INDAT[,mergevar:=1:.N] # Variable to merge by after split;
    setnames(INDAT,invars,c("pnr","inn","out","event"))
    ## if (!(class(tolower(INDAT[,inn])) %in% c("numeric","date","integer")) | !(class(tolower(INDAT[,out])) %in% c("numeric","date","integer"))) stop(paste("dates in",indat,"not numeric")) 
    setkey(INDAT,pnr)
    INDAT[,pnrnum:=.GRP,by="pnr"] # Number pnr - As a consecutive sequence
    pnrgrp <-unique(INDAT[,c("pnr","pnrnum"),with=FALSE]) 
    RESTDAT <- copyindat[,(invars[2:4]):=NULL]# Other variables to be added at end
    RESTDAT[,mergevar:=1:.N]  # Merge after split assuming possible prior splits 
    setnames(RESTDAT,invars[1],"pnr")
    #Prepare splitdat
    csplit <- copy(splitdat)
    csplit[,splitvars,with=FALSE] # necessary variables
    setnames(csplit,c("pnr","start","slut","val","name"))
    setkey(csplit,"pnr")
    csplit <- merge(csplit,pnrgrp,by="pnr")
    csplit[,pnr:=NULL] # identify only by pnrnum
    setkeyv(csplit,c("name","pnrnum","start","slut")) 
    ## Check csplit content
    temp <- csplit[start>slut,sum(start>slut)]
    if (temp>0) stop(paste("Error - Attempt to split with negative date intervals in splitting guide"))
    precols = paste("prior", c("start","slut"), sep="_")
    cols <-c("start","slut")
    csplit[, (precols) := shift(.SD, 1, 0, "lag"), .SDcols=cols,by=c("name","pnrnum")]
    error <-dim(csplit[start-prior_slut<0,])[1]
    if (error>0) stop("Error in splitting guide data - Intervals overlapping - unpredictable results")
    suppressWarnings(csplit[,precols:=NULL])#remove extra variables
    #Separate tables for each value of name
    namelist <- unique(csplit[["name"]])
    OUT <- INDAT[,c("pnrnum","mergevar","inn","out","event"),with=FALSE] # Prepare output start
    OUT[,mergevar2:=mergevar] # to add sequential information
    PRIOR <- OUT[,c("mergevar","mergevar2")]
  
  for(nam in namelist){ # Separate splitting for each "name"
    .pnrnum <- OUT[["pnrnum"]]
    .inn <- OUT[["inn"]]
    .out <- OUT[["out"]]
    .event <- OUT[["event"]]
    .mergevar2 <- OUT[["mergevar2"]]
    
    .Spnrnum <- csplit[name==nam][["pnrnum"]]
    .val <- as.character(csplit[name==nam][["val"]])
    .start <- csplit[name==nam][["start"]]
    .slut <- csplit[name==nam][["slut"]]
    OUT2 <- .Call('_heaven_splitFT',PACKAGE = 'heaven',.pnrnum, .inn, .out, .event, .mergevar2,.Spnrnum, .val, .start, .slut) # Call c++
    #OUT2 <- splitFT(.pnrnum, .inn, .out, .event, .mergevar2,.Spnrnum, .val, .start, .slut)
    setDT(OUT2) # which now has more records and a new colume named after name
    setnames(OUT2,c("merge","val"),c("mergevar2",nam))
    setkeyv(OUT2,c("mergevar2","inn"))
    setkey(PRIOR,mergevar2)
    OUT <- merge(OUT2,PRIOR,by="mergevar2",all=TRUE) #Add information from prior loops
    OUT[,mergevar2:=1:.N] # Resequence
    PRIOR <- copy(OUT)
    PRIOR[,c("pnrnum","inn","out","event"):=NULL] #Remove data provided by splitFT function
  }
  setkey(OUT, mergevar, inn)
  setkey(RESTDAT, mergevar)
  OUT <- merge(OUT,RESTDAT, by=c("mergevar"),all=TRUE)
  setnames(OUT,c("pnr","inn","out","event"),invars) 
  OUT[,c("mergevar","mergevar2","pnrnum"):=NULL]
  OUT[]
}
