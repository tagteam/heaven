#' @title splitTwo
#' @description
#' This function can split records from a dataset in two records using
#' information from a "splitting-guide". At start 
#' each record has variables representing stand and end of time - after the
#' split time of a split record end at the time of a split one one record and
#' represent the start of time on the next record.
#' 
#' The dataset to be split needs to have variables that define start and
#' end of a time interval (start/end).  Also necessary is a variable (id) that 
#' defines the relation between individuals and records in both the dataset
#' to be split and the splitting guide.
#' 
#' The dataset to be split can have any other variable than those necessary
#' for the split - these appear on each of the split records after the split.
#' 
#' The splitting-guide provides the information on which times to split records.
#' This guide can be provide in a wide or a long format:
#' 
#' Wide format:
#' This requires one record pr. individual that have dates to be split on.  
#' One column defines the same id as in the "base" table. The other columns 
#' contain dates for each condition where the split should occur.  These column 
#' names will also appear in the output data, but on output the values are zero 
#' before the dates and 1 after.  When dates are NA output has zero. 
#' 
#' Long format:
#' This requires one record per data where a possible split should occur. The
#' columns should contain id, name of condition and the data to split on. 
#' Output is identical to that provide with the wide format.
#' 
#' @usage
#' splitTwo(indat,splitdat,invars,splitvars,format="wide",datacheck=TRUE)
#' @author Christian Torp-Pedersen
#' @param indat A data.table or data.frame whose first 3 columns are:
#' \itemize{
#' \item{id}{ Person identification variable such as PNR. The data may contain 
#' multiple lines per subject.}
#' \item{start}{ Start of time interval. Either a date or an integer/numeric.}
#' \item{end}{   End of time interval. Either in date format or given as 
#' numeric/integer.}
#' }
#' @param splitdat The splitting guide. A data.table which contains person 
#' specific information about the onset dates of comorbidities and other events.
#' Wide format:
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
#' \item{Dat3 ....}
#' }
#' Long format:
#' \itemize{
#' \item{id}{ Person identification variable such as PNR. The data may contain 
#' multiple lines per subject.}
#' \item{Condition name}{Character providing the variable name of condition} 
#' \item{Date}{ Either a date or an integer/numeric. The onset date of 
#' comorbidity or other event. If integer/numeric it can be time since a 
#' baseline date on project specific scale (e.g., days or months).}
#' }
#' 
#' @param invars vector of column names for id/entry/exit - in that 
#' order, example: c("id","start","end")
#' @param splitvars
#' For wide format: 
#' - vector of column names of columns containing dates to 
#' split by. example: c("date1","date2","date3","date4")
#' For long format: 
#' - vector of the 3 columns in the data.table: id/name/date,
#' example: c("id","name","date")
#' The name of the id column must be the same in both datasets
#' @param format - format of splitting guide - "wide"  or "long"
#' @param datacheck - This program may crash if intervals are overlapping or
#' negative. Data checking produces an error in such cases. This can be omitted 
#' if the data have been checked by other means.  For the splitting guide 
#' this options checks that there is only one entry for each variable to 
#' split by for each person identifier.
#' @return
#' The function returns a new data table where records have been split according 
#' to the splitting guide dataset. Variables unrelated to the splitting are 
#' left unchanged. The names of columns from "splitvars" are also in output
#' data, but now they have the value zero before the dates and 1 after.
#' @export
#' @details 
#' The program checks that intervals are not negative and that intervals from 
#' one individual do not overlap.
#' 
#' It is required that the splitting guide contains at least one record.  
#' Missing data in the person id variables are not allowed and will cause errors.
#' 
#' A note of caution: This function works with dates as numeric. R has a default
#' original of dates as 1 January 1970, but other programs have different
#' default origins - and this includes SAS and Excel. It is therefor important
#' for decent results that care is taken that all dates are defined similarly.
#' 
#' The output will always have the "next" period starting on the day where the
#' last period ended. This is to ensure that period lengths are calculated pro-
#' perly. The program will also allow periods of zero lengths which is a conse-
#' quence when multiple splits are made on the same day. 
#' 
#' @examples
#' library(data.table)
#' 
#' dat <- data.table(pnr=c("123456","123456","234567","234567","345678","345678"
#' ,"456789","456789"),
#'                 start=as.Date(c(0,100,0,100,0,100,0,100),origin="1970-01-01"),
#'                 end=as.Date(c(100,200,100,200,100,200,100,200),origin="1970-01-01"))
#'                 
#' split <- data.table (pnr=c("123456","234567","345678","456789"),
#' como1.onset=as.integer(c(0,NA,49,50)), como2.onset=as.integer(c(25,75,49,49)),
#' como3.onset=as.integer(c(30,NA,49,48)), como4.onset=as.integer(c(50,49,49,47))) 
#' #Show the datasets:
#' dat[]
#' split[]
#' splitTwo(dat # inddato with id/in/out
#'    ,split # Data with id and dates
#'    ,c("pnr","start","end") #names of id/in/out - in that order
#'    ,c("pnr","como1.onset","como2.onset","como3.onset","como4.onset")) 
#'    #Names of date-vars to split by
#' # And with splittingguide in long format
#' splitvars <- c("como1.onset","como2.onset","como3.onset","como4.onset")
#' split <- data.table::melt(data=split,id.vars="pnr",measure.vars=splitvars,
#'   variable.name="name",value.name="value")
#' split[,value:=as.Date(value,origin="1970-01-01")]
#' split[]
#' split <- split[!is.na(value)] # remove missing values
#' splitTwo(dat # in-data with id/in/out
#'    ,split # Data with id/name/date
#'    ,c("pnr","start","end") #names of id/in/out - in that order
#'    ,c("pnr","name","value")
#'    ,format="long") 
#' @export

splitTwo <- function(indat, # in-data with id/in/out - and possibly other variables
                     splitdat, # Data with id and dates
                     invars, #names of id/in/out - in that order
                     splitvars, #Names var date-vars to split by
                     format="wide", # Wide or long format of splitting guide
                     datacheck=TRUE #Check consistensy of data
){
  .N=inn=out=.SD=dato=pnrnum=mergevar=.GRP=pnr=number_=value_=value=num=numcov=name=isdate=NULL
  datt <- copy(indat)
  splitdatt <- copy(splitdat)
  #Tests of data
  setDT(datt)
  setDT(splitdatt)
  datt[,mergevar:=1:.N] # var to merge RESTDAT on later - assuming data may have been presplit with multiple lines with pnr
  RESTDAT <- copy(datt)[,(invars[2:3]):=NULL]# non-split variables to be added at end
  setnames(RESTDAT,invars[1],"pnr")
  datt <- datt[,c("mergevar",invars),with=FALSE] # Ncessary variables for split
  setnames(datt,invars,c("pnr","inn","out"))
  if (lubridate::is.Date(datt[,inn])) {
    datt[,':='(inn=as.numeric(inn),out=as.numeric(out))]
    isdate <- TRUE
  }
  else isdate <- FALSE
  ## TEST
  if(datacheck){
    temp <- datt[,list(num=sum(out<inn))]
    if (temp[,num]>0) stop("Error - end of interval cannot come before start of intervals")
    if (!class(datt[,inn]) %in% c("numeric","integer","Date") | !class(datt[,out]) %in% c("numeric","integer","Date")) 
      stop("input date not Date or integer or numeric")
    setkeyv(datt,"inn")
    temp <- datt[,list(num=sum(inn<shift(out,fill=inn[1]))),by="pnr"]
    temp <- temp[,list(num=sum(num))]
    if(temp[,num]>0) stop("Error - Data includes overlapping intervals")
  }
  # Create consecutive increasing replacement from pnr in datt and splitdatt
  setkey(datt,"pnr")
  datt[,pnrnum:=.GRP,by="pnr"] #Numeric replacement for pnr
  setnames(splitdatt,invars[1],"pnr")
  splitdatt<-merge(splitdatt,unique(datt[,c("pnr","pnrnum"),with=FALSE]),by="pnr",all.x=TRUE) # Same pnrnum as in datt
  splitdatt <- splitdatt[!is.na(pnrnum)]
  splitdatt[,pnr:=NULL] #remove pnr
  # Create long-form of splitdatt and number covariate dates
  if (format=="wide"){
    if(datacheck){
      temp <- splitdatt[,pnrnum]
      if (length(temp)!=length(unique(temp))) stop("Error - Repeated entries in splitting guide")
    }
    splitvars2 <- splitvars[-1]
    setcolorder(splitdatt,c("pnrnum",splitvars2)) # Columns ordered as in call
    splitdatt <- data.table::melt(data=splitdatt,id.vars="pnrnum",measure.vars=splitvars2,variable.name="_variable_",value.name="value_")
    setkeyv(splitdatt,"pnrnum")
    splitdatt[,"_variable_":=NULL]
    splitdatt[,number_:=1:.N,by="pnrnum"] 
    splitdatt <- splitdatt[!is.na(value_)]
    splitdatt[,numcov:=.N,by="pnrnum"]
    setkeyv(splitdatt,c("pnrnum","value_"))
    splitdatt[,value_:=as.numeric(value_)]
    splitdatt <- as.matrix(splitdatt) 
  } else { # format=long
    if(datacheck){
      setkeyv(splitdatt,"pnrnum")
      splitdatt[,numvars:=.N,by="pnrnum"]
      temp <-splitdatt[,list(numvars=max(numvars))]
      if (temp[,numvars]>length(unique(splitdatt[,name])))stop("Error - Apparent repeated entries in splitting guide")
      splitdatt[,numvars:=NULL]
    }
    setnames(splitdatt,splitvars[2:3],c("name","value_"))
    splitvars <- as.character(unique(splitdatt[,"name"])$name)
    splitvars2 <- splitvars
    toNumber <- data.table(name=splitvars,number_=1:length(splitvars))
    splitdatt <- merge(splitdatt,toNumber,by="name",all.x=TRUE) # attach names of splitvars
    splitdatt[,name:=NULL]
    #Number of covariates for each case
    splitdatt[,numcov:=.N,by="pnrnum"]
    setkeyv(splitdatt,c("pnrnum","number_"))
    if(datacheck){
      # Check for repeated "name"/number in one pnr
      temp <- splitdatt[shift(pnrnum)==pnrnum & shift(number_)==number_,.SD,.SDcols="pnrnum",by="pnrnum"]
      if (dim(temp)[1]>=1) stop("Error - repeated split variables in at least one id-group")
    }
    setkeyv(splitdatt,c("pnrnum","value_"))
    setcolorder(splitdatt,c("pnrnum","value_","number_","numcov"))
    splitdatt[,value_:=as.numeric(value_)]
    splitdatt <- as.matrix(splitdatt)
  }
  datt[,event:=0] # Dummy to match c++ function
  #OUT <- .Call("_heaven_split2",PACKAGE = "heaven",datt[,pnrnum],datt[,inn],datt[,out],datt[,mergevar],splitdatt,length(splitvars))  # Call to c++ split-function
  OUT <- split2(datt[,pnrnum],datt[,inn],datt[,out],datt[,event],datt[,mergevar],splitdatt,length(splitvars2))  # Call to c++ split-function
  OUT <- cbind(setDT(OUT[1:5]),setDT(do.call(cbind,OUT[6])))
  if(isdate){
    OUT[,':='(inn=as.Date(inn,origin="1970-01-01"),out=as.Date(out,origin="1970-01-01"))]
  }
  OUT[,event:=NULL]  
  setnames(OUT, c("pnrnum","mergevar",invars[2:3],splitvars2))
  OUT <- merge(OUT,
               RESTDAT,by="mergevar")
  setnames(OUT,"pnr",invars[1])
  OUT[,c("pnrnum","mergevar"):=NULL] # remove number version of pnr
  OUT[]
}

