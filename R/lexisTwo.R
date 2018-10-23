#' @title lexisTwo
#' 
#' @description
#' Splitting is about collecting person specific exposure-outcome-confounder
#' pattern over time in start-stop-event format.
#' lexixTwo is one of 3 splitting functions in heaven.
#' lexixTwo is useful to add the time-dynamic information about comorbidities
#' and other events in binary (yes/no) format 
#' to an existing data set which readily contains person specific information
#' in start-stop-event format. A person specific time interval (start-stop) of 
#' the existing data set is split according to the occurrence dates of the 
#' comorbidities and other events whenever the comorbidity status (event status)
#' changes within the time interval. 
#' 
#' The "base" data are the data to be split. They 
#' may contain much information, but the key is "id","start","end" and "event". 
#' These describe the participant's id, start of time interval, end of time
#' interval and the event of interest (must be 0/1).
#' 
#' The other input is a data.table with the splittingguide. This data.table 
#' should have one record pr. individual.  One column defined the same id as in
#' the "base" table. The other columns contain dates for each condition where 
#' the split should occur.  These column names will also appear in the output
#' data, but on output the values are zero before the dates and 1 after.  When
#' dates are NA output has zero. 
#' @usage
#' lexisTwo(indat,splitdat,invars,splitvars)
#' @author Christian Torp-Pedersen
#' @param indat A data.table or data.frame whose first 4 columns are in that 
#' order:
#' \itemize{
#' \item{id}{ Person identification variable such as PNR. The data may contain 
#' multiple lines per subject.}
#' \item{start}{ Start of time interval. Either a date or an integer/numeric.}
#' \item{end}{ End of time interval. Either in date format or given as 
#' numeric/integer.}
#' \item{event}{ Binary 0-1 variable indicating if an event occurred at end of 
#' interval}
#' }
#' @param splitdat The splittingguide. A data.table which contains person 
#' specific information about the onset dates of comorbidities and other events.
#' \itemize{
#' \item{id}{ Person identification variable such as PNR. The data may contain 
#' multiple lines per subject.}
#' \item{Date 1}{ Either a date or an integer/numeric. Format must match that of 
#' the start and stop of arguments 
#' \code{indat} The onset date of comorbidity 1 or other event. If 
#' integer/numeric it can be time since a baseline date on project specific 
#' scale (e.g., days or months).}
#' \item{Date 2}{ Either a date or an integer/numeric. The onset date of 
#' comorbidity 2 or other event. If integer/numeric it can be time since a 
#' baseline date on project specific scale (e.g., days or months).}
#' }
#' @param invars vector of column names for id/entry/exit/event - in that 
#' order, example: c("id","start","end","event")
#' @param splitvars - vector of column names of columns containing dates to 
#' split by. example: c("date1","date2","date3","date4")
#' The name of the id column must be the same in both datasets
#' @return
#' The function returns a new data table where records have been split according 
#' to the splittingguide dataset. Variables unrelated to the splitting are 
#' left unchanged. The names of columns from "splitvars" are also in output
#' data, but now they have the value zero before the dates and 1 after.
#' @export
#' @details 
#' The program checks that intervals are not negative. Violation results in
#' an error. Overlap may occur in real data, but the user needs to make 
#' decisions regarding this prior to using this function.
#' 
#' It is required that the splittingguide contains at least one record.  
#' Missing data in the person id variables are not allowed and will cause errors.
#' 
#' A note of caution: This function works with dates as integers. R has a default
#' original of dates as 1 January 1970, but other programs have different
#' default origins - and this includes SAS and Excel. It is therefor important
#' for decent results that care is taken that all dates are defined similarly.
#' 
#' The output will always have the "next" period starting on the day where the
#' last period ended. This is to ensure that period lengths are calculated pro-
#' perly. The program will also allow periods of zero lengths which is a conse-
#' quence when multiple splits are made on the same day. When there is an event
#' on a period with zero length it is important to keep that period not to 
#' loose events for calculations. Whether other zero length records should be
#' kept in calculations depends on the context.
#' @seealso lexisSeq lexisFromTo
#' @examples
#' library(data.table)
#' 
#' dat <- data.table(pnr=c("123456","123456","234567","234567","345678","345678"
#' ,"456789","456789"),
#'                 start=as.integer(c(0,100,0,100,0,100,0,100)),
#'                 end=as.integer(c(100,200,100,200,100,200,100,200)),
#'                 event=as.integer(c(0,1,0,0,0,1,0,1)))
#' split <- data.table (pnr=c("123456","234567","345678","456789"),
#' como1.onset=as.integer(c(0,50,150,300)), como2.onset=as.integer(c(25,75,175,325)),
#' como3.onset=as.integer(c(30,30,30,30)), como4.onset=as.integer(c(0,1,0,1)))
#' lexisTwo(dat # inddato with id/in/out/event
#'    ,split # Data with id and dates
#'    ,c("pnr","start","end","event") #names of id/in/out/event - in that order
#'    ,c("como1.onset","como2.onset","como3.onset","como4.onset")) 
#'    #Names of date-vars to split by
#' @export
lexisTwo <- function(indat # inddato with id/in/out/event - and possibly other variables
                      ,splitdat # Data with id and dates
                      ,invars #names of id/in/out/event - in that order
                      ,splitvars #Names var date-vars to split by
){
  .N=inn=out=dead=.SD=dato=pnrnum=NULL
  #Tests of data
  copyindat <- copy(indat) # leave original dataset intact
  setDT(copyindat)
  copysplitdat <- copy(splitdat)
  setDT(copysplitdat)
  copyindat[,pnrnum:=1:.N] # var to merge RESTDAT on later - assuming data have been presplit with multiple lines with pnr
  INDAT <- copyindat[,c("pnrnum",invars),with=FALSE] # Ncessary variables for split
  setnames(INDAT,invars,c("pnr","inn","out","dead"))
  ## TEST
  if (!class(INDAT[,inn]) %in% c("integer","Date") | !class(INDAT[,out]) %in% c("integer","Date")) 
         stop("inpute date not Date or integer")
  if (!class(INDAT[,dead])=="integer") stop ("Event variable not integer")
  ## if (!class(tolower(INDAT[,inn])) %in% c("numeric","date","integer") | !class(tolower(INDAT[,out])) %in% c("numeric","date","integer")) stop(paste("dates in",indat,"not numeric")) 
  RESTDAT <- copyindat[,(invars[2:4]):=NULL]# Other variables to be added at end
  setnames(RESTDAT,invars[1],"pnr")
  OUT <- INDAT[,c("pnrnum","inn"),with=FALSE] # Prepare output start
  setnames(copysplitdat,invars[1],"pnr")
  for(name in splitvars){
    selected <- na.omit(copysplitdat[,c("pnr",name),with=FALSE])
    toSplit <- merge(INDAT,selected,by="pnr",all.x=TRUE)
    pnrmerge <- unique(INDAT[,c("pnr","pnrnum"),with=FALSE])# relation between pnr and pnrnum
    if (name != splitvars[1]) OUT[,(c("out","dead")):=NULL]
    #INDAT <- toSplit[,split22(pnrnum,inn,out,eval(as.name(name)),dead)]   
    INDAT <- toSplit[,.Call('_heaven_split2',PACKAGE = 'heaven',pnrnum,inn,out,dead,eval(as.name(name)))]  # Call to c++ split-function
    setDT(INDAT)
    INDAT <- merge(INDAT,pnrmerge,by="pnrnum",all.x=TRUE)
    OUT <- merge(INDAT,OUT,by=c("pnrnum","inn"),all=TRUE) 
    OUT <- OUT[,.SD[.N],by=c("pnrnum","inn","out")]
    OUT[,"pnr":=NULL]
    INDAT[,dato:=NULL]
    setnames(OUT,"dato",name)
  }
  OUT[,(splitvars):=lapply(.SD,function(x){.Call('_heaven_na_locf',PACKAGE = 'heaven',x)}), 
      by = "pnrnum", .SDcols = splitvars ]
#  OUT[,(splitvars):=lapply(.SD,heaven::na_locf),by = "pnrnum", .SDcols = splitvars ]
  OUT <- merge(OUT,RESTDAT,by="pnrnum")
  OUT[,pnrnum:=NULL] # remove number version of pnr
  setnames(OUT,c("pnr","inn","out","dead"),invars)
  OUT[]
}
