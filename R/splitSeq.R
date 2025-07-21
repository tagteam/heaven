#' @title splitSeq
#' @description 
#' splitSeq is a function which can split records according to a vector of
#' selected times.  At the outset each record has two variables representing
#' start and end on a time scale. A vector of time points is supplied and each
#' record is replaced by as many records as the number of times points from the
#' vector that occurs in the interval.  After splitting the variable
#' representing end of time is replaced by the splitting-time and the next
#' record has this splitting-time as the start of time variable.
#' 
#' This function is particularly useful to split variables according to 
#' variables that change continuously. Typical situations are age(e.g. 5 year
#' periods), calender time (e.g. 2 year periods) and selected times after a
#' situation of interest (e.g. fixed sized time periods after a starting date). 
#' The input is a data.table and splitting guide.  The "base" data are the 
#' data to be split. They may contain much information, but the key is "id",
#' "start" and "end". These describe the participant's id, start of time 
#' period and end of time period.
#' 
#' The other input is data to define splitvector and name. The splitvector may
#' be a fixed vector (format="vector", e.g. a series of fixed calender dates) or 
#' a list of 3 integers defining start, end and interval to split by 
#' (format="seq", for a split on age between 20 and 80 by 5 years a splitvector 
#' could be defined as: 
#' splitvector <- c(20,80,5)*365.25 and provided to the function as a variable).
#' "varname" is a name of a variable in the data.table the defines a value to be
#' added to the splitvector.  For the age split just used as an example it would
#' be a variable containing the birth date. For a split after onset of a
#' conditi on it should be the date of the condition and NA when the condition 
#' does not occur.  When no value should be added to the vector (e.g. split by 
#' calender time) "varname" should keep its default value of NULL.
#' 
#' On output a new variable with default name "value" defines the result of 
#' splitting. The variable can be renamed to a user defined name (e.g.
#' value="myvalue"). This variable will contain zero when time is before the
#' first value of the splitting vector (added the "varname") and then increased
#' by one as each value of the splitting vector is reached.
#' 
#' @usage
#' splitSeq(indat,invars,varname=NULL,splitvector,format,value="value",
#' datacheck=TRUE)
#' @author Christian Torp-Pedersen
#' @param indat base data with id, start, end and other data - possibly 
#' already split
#' @param invars column names for id,entry,exit - in that 
#' order, example: c("id","start","end")
#' @param varname name of variable to be added to vector
#' @param splitvector A vector of calender times (integer). Splitvector is
#' a sequence of fixed dates (or other time scala).
#' @param format String with two possible values:
#' \itemize{
#' \item \code{"vector"} a series of fixed calender dates
#' \item \code{"seq"} see description
#' }
#' @param value 0 to the left of the vector, increase of 1 as each element of 
#' vector is passed
#' @param datacheck - Checks that data are in appropriate format and that 
#' intervals are neihter negative or overlapping. Can be set to FALSE if checked
#' elsewhere.
#' @return
#' The function returns a new data table where records have been split according 
#' to the values in splitvector. Variables unrelated to the splitting are left 
#' unchanged.
#' @export
#' @details 
#' The input must be data.table.  This data.table is assumed already to be split 
#' by other functions with multiple records having identical participant id. 
#' The function extracts those variables necessary for splitting, splits
#' by the provided vector and finally merges other variable onto the final 
#' result.
#' 
#' A note of caution: This function works with dates as integers. R has a de-
#' fault origina of dates as 1 January 1970, but other programs have different
#' default origins - and this includes SAS and Excell. It is therefor important
#' for decent results that care is taken that all dates are defined similarly.
#' 
#' The output will always have the "next" period starting on the day where the
#' last period ended. This is to ensure that period lengths are calculated pro-
#' perly. The program will also allow periods of zero lengths which is a conse-
#' quence when multiple splits are made on the same day. When there is an event
#' on a period with zero length it is important to keep that period not to 
#' loose events for calculations. Whether other zero length records should be
#' kept in calculations depend on context.
#' 
#' This function is identical to the lexisSeq function with the change that 
#' "event" is not considered.
#' @seealso lexisSeq
#' @examples
#' library(data.table)
#' 
#' dat <- data.table(ptid=c("A","A","B","B","C","C","D","D"),
#'                 start=as.Date(c(0,100,0,100,0,100,0,100),origin="1970-01-01"),
#'                 end=as.Date(c(100,200,100,200,100,200,100,200),origin="1970-01-01"),
#'                 Bdate=as.Date(c(-5000,-5000,-2000,-2000,0,0,100,100),origin="1970-01-01"))
#' #Example 1 - Splitting on a vector with 3 values to be added to "Bdate"                 
#' out <- splitSeq(indat=dat,invars=c("ptid","start","end"),
#'                varname="Bdate",
#'                splitvector=as.Date(c(0,150,5000),origin="1970-01-01"),
#'                format="vector")
#' out[]
#' #Example 2 - splitting on a from-to-by sequence with no adding (calender time?)
#' out2 <- splitSeq(indat=dat,invars=c("ptid","start","end"),
#'                  varname=NULL,splitvector=c(0,200,50),
#'                  format="seq",value="myvalue")
#' out2[]
#' @export
splitSeq <- function(indat,
                     invars,
                     varname = NULL,
                     splitvector,
                     format,
                     value = "value",
                     datacheck=TRUE) 
{
  vent = out = inn = .SD = pnrnum = .N = isdate= NULL
  if (datacheck){
    if (class(invars) != "character") 
      stop("Varnames in c(..) not character")
    if (class(varname) != "character" & !is.null(varname)) 
      stop("varname not character or NULL")
  }
  datt <- data.table::copy(indat)
  data.table::setDT(datt)
  if (is.null(varname)) 
    datt[, `:=`(varname, 0)]
  else {
    setnames(datt, varname, "varname")
    datt[is.na(varname),varname:=as.Date("3000-01-01")] # Make missing varname very large
  }
  datt[, `:=`(pnrnum, 1:.N)]
  splitdat <- datt[, .SD, .SDcols = c("pnrnum", invars[2:3], 
                                      "varname")]
  setnames(splitdat,c("pnrnum", invars[2:3],"varname"), 
           c("pnrnum", "inn", "out", "varname"))
  if (lubridate::is.Date(splitdat[,inn])){
    splitdat[,':='(inn=as.numeric(inn),out=as.numeric(out))]
    isdate <- TRUE
  }
  else isdate <- FALSE
  if (datacheck) {
    temp <- splitdat[,list(num=sum(out<inn))]
    if (temp[,num]>0) stop("Error - end of intervald cannot come before start of intervals")
    if (!class(splitdat[,inn]) %in% c("numeric","integer","Date") | !class(splitdat[,out]) %in% c("numeric","integer","Date")) 
      stop("input date not Date or integer or numeric")
    setkeyv(splitdat,"inn")
    temp <- splitdat[,list(num=sum(inn<shift(out,fill=inn[1]))),by="pnrnum"]
    temp <- temp[,list(num=sum(num))]
    if(temp[,num]>0) stop("Error - Data includes overlapping intervals")
    datt[, `:=`((invars[2:3]), NULL)]
    if (!(format %in% c("vector", "seq"))) 
      stop("format must be 'seq' or 'vector'")
    if (format == "seq") {
      if ((length(splitvector) != 3) || (splitvector[1] >= 
                                         splitvector[2]) || (splitvector[3] >= (splitvector[2] - 
                                                                                splitvector[1]))) 
        stop("Argument 'seq' must be a vector of the form (start, stop, by) where start < stop and by < stop-start.")
      splitguide <- seq(splitvector[1],splitvector[2],splitvector[3]) 
    }  
    else {
      if (length(splitvector)>1) 
        for (i in 2:length(splitvector))
          if (splitvector[i]<=splitvector[i-1]) stop("Splitvector not with increasing numbers")
      splitguide <- splitvector     
    }
  }
  splitdat[,event:=0] # Dummy to fit c++ function
  out <- splitdat[,splitDate(inn, out, event, pnrnum, splitguide, varname)]  
  setDT(out)
  out[,event:=NULL] # Dummy removed
  setkeyv(out, c("pnrnum", "inn"))
  if(isdate){
    out[,':='(inn=as.Date(inn,origin="1970-01-01"),out=as.Date(out,origin="1970-01-01"))]
  }
  if (is.null(varname)) 
    datt[, `:=`(varname, NULL)]
  else setnames(datt,"varname",varname)
  out <- merge(out, datt, by = "pnrnum", all = TRUE)
  out[, `:=`(pnrnum, NULL)]
  setnames(out, c("inn", "out", "value"), c(invars[2:3], 
                                                     value))
  out[]
}
