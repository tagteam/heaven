#' @title lexisTwo
#' 
#' @description 
#' lexixTwo is a specialised version of lexis splitting.  While preparing data for time dependent analyses with e.g.
#' Cox org Poisson regression records need to be "split" at selected times.  For practical use there are often
#' multiple conditions such as comorbidities that will cause either a single split or no splitting.  This function
#' is designed to receive base data with start/end/event and another set of data where patient id is accompanied with 
#' one column per condition with these columns containing the time where split should occur.
#' @usage
#' lexisTwo(indat,splitdat,invars,splitvars)
#' @author Christian Torp-Pedersen
#' @param indat - base data with id, start, end, event and other data - possibly already split
#' @param splitdat - Data with splitting guide - id and columns with dates to split by 
#' @param invars - vector of colum names for id/entry/exit/event - in that order, 
#' example: c("id","start","end","event")
#' @param splitvars - vector of column names of columns containing dates to split by.
#' example: c("date1","date2","date3","date4")
#' The name of the id column must be the same in both datasets
#' @return
#' The function returns a new data table where records have been split according to the splittingguide dataset. Variables
#' unrelated to the splitting are left unchanged.
#' @export
#' @details 
#' The input to this function are two data.tables and two lists of the critical variables.  The base data it the data to be split.
#' This data must have a variable to identify participants, start/end times and a variable to indicate event after last interval.
#' The other table contains a single record for each participant and columns to identify dates to split by.  After splitting all
#' intervals preceding the date will have a variable identified by each column with the value "0". After the date the value i "1".
#' In the example the columns are dat1-date4 - but it is most useful to provide names that identify the condition which changes by
#' the date rather than a names which indicates a date.
#' @seealso lexisSeq lexisFromTo
#' @examples
#' library(data.table)
#' dat <- data.table(id=c("A","A","B","B","C","C","D","D"),
#'                 start=c(0,100,0,100,0,100,0,100),
#'                 end=c(100,200,100,200,100,200,100,200),
#'                 event=c(0,1,0,0,0,1,0,1))
#' split <- data.table (id=c("A","B","C","D"),date1=c(0,NA,150,300),date2=c(25,75,175,325),date3=c(30,30,175,325),
#'                     date4=c(0,1,0,1))
#' temp <- lexisTwo(dat # inddato with id/in/out/event
#'        ,split # Data with id and dates
#'        ,c("id","start","end","event") #names of id/in/out/event - in that order
#'        ,c("date1","date2","date3","date4")) #Names var date-vars to split by
#' temp[]
#' @export
lexisTwo <- function(indat # inddato with id/in/out/event - and possibly other variables
                     ,splitdat # Data with id and dates
                     ,invars #names of id/in/out/event - in that order
                     ,splitvars #Names var date-vars to split by
                     ){
  #Tests of data

  if (!is.data.table(indat) | !is.data.table(splitdat)) stop("Input not data tables")
  copyindat <- copy(indat) # leave original dataset intact
  copyindat[,pnrnum:=1:.N] # var to merge RESTDAT on later - assuming data have been presplit with multiple lines with pnr
  copysplitdat <- copy(splitdat)
  INDAT <- copyindat[,c("pnrnum",invars),with=FALSE] # Ncessary variables for split
  setnames(INDAT,invars,c("pnr","inn","out","dead"))
  RESTDAT <- copyindat[,(invars[2:4]):=NULL]# Other variables to be added at end
  setnames(RESTDAT,invars[1],"pnr")
  OUT <- INDAT[,c("pnrnum","inn"),with=FALSE] # Prepare output start
  MAX <- INDAT[,max(out,na.rm=TRUE)] # find max date - to handle missing dates
  setnames(copysplitdat,invars[1],"pnr")
  # Replace missing dates with MAX+1:
  for (j in seq_len(ncol(copysplitdat)))
    set(copysplitdat,which(is.na(copysplitdat[[j]])),j,MAX+1)
  for(name in splitvars){
    selected <- copysplitdat[,c("pnr",name),with=FALSE]
    toSplit <- merge(INDAT,selected,by="pnr",all.x=TRUE)
    pnrmerge <- unique(INDAT[,c("pnr","pnrnum"),with=FALSE])# relation between pnr and pnrnum
    if (name != splitvars[1]) OUT[,(c("out","dead")):=NULL]
   # INDAT <- heaven::split2(.pnr,.in,.out,.dato,.event) 
    INDAT <- toSplit[,.Call('_heaven_split2',PACKAGE = 'heaven',pnrnum,inn,out,eval(as.name(name)),dead)]  # Call to c++ split-function
    setDT(INDAT)
    INDAT <- merge(INDAT,pnrmerge,by="pnrnum",all.x=TRUE)
    OUT <- merge(INDAT,OUT,by=c("pnrnum","inn"),all=TRUE) 
    OUT <- OUT[,tail(.SD,1),by=c("pnrnum","inn","out")]
    OUT[,"pnr":=NULL]
    INDAT[,dato:=NULL]
    setnames(OUT,"dato",name)
  }
 OUT[,(splitvars) := zoo::na.locf(.SD, na.rm = F), by = "pnrnum", .SDcols = splitvars]  
 OUT <- merge(OUT,RESTDAT,by="pnrnum")
 OUT[,pnrnum:=NULL] # remove number version of pnr
 setnames(OUT,c("pnr","inn","out","dead"),invars)
 OUT
}