#' @title averageIncome
#' @description
#' This function calculates average income for a range of years prior to a date.
#' The function expects a data.table with yearly incomes from which to calcu-
#' late the average.
#' 
#' The function can include multiple occurrences of a patient ID, but output is 
#' restricted to one average income calculated for each combination of person
#' and year.
#' 
#' If the input data includes income from many years not relevant to the project,
#' then it is wise to trim these data prior to using the function.
#' @usage averageIncome(data,income,datvars,incomevars,numyears=5)
#' @author Christian Torp-Pedersen 
#' @param data - A dataframe/table with with an identification variable and a
#' date from which the previous years' income should be averaged.
#' @param income - A dataframe/table with at least three variables which are the 
#' identification of individual, the year and the income
#' @param datvars - A character vector of two: identification and the date 
#' representing the year that is just after the years used to calculate
#' average income.
#' @param incomevars - A character with three variables: identification variable,
#' year and income - in that order
#' @param numyears - the number of years to include in the average income,
#' default is 5.
#' @details
#' The function can in particular be used with the income data provided by
#' Statistics Denmark. These files include a record per year and sometimes a
#' record by quarter. The function will assume that repeated incomes for a year
#' should be removed leaving a single income per year. The actual income 
#' selected for averaging is chosen by the user when selecting variables.
#' @return a data.table with 2 records, the identification variable and the 
#' averaged income.
#' @examples
#' require(data.table)
#' dat <- data.table(pnr=c(1,2,3,3), dato=as.Date(paste0(2018:2021,"-11-11")))
#' indkomst <- data.table(pnr=rep(c(1,2,3,3),5),year=rep(2015:2020,5),
#' income=rep(200:205,5))
#' averageIncome(dat,indkomst,c("pnr","dato"),c("pnr","year","income"))
#' @export
averageIncome <- function(data,income,datvars,incomevars,numyears=5){
browser()  
  if(!"data.frame" %in% class(data)) stop("First variable must be a data.frame or data.table")
  if(!"data.frame" %in% class(income)) stop("First variable must be a data.frame or data.table")
  if(!class(datvars)=="character" | !length(datvars)==2) stop("datvars must be a character vector of two")
  if(!class(incomevars)=="character" | !length(incomevars)==3) stop("incomevars must be a character vector of three")
  year=yearinc=.SD= .=NULL 
  dat <- data[,.SD,.SDcols=datvars]
  data.table::setDT(dat)
  data.table::setnames(dat,c("ID","year"))
  dat[,year:=year(year)]
  dat <- unique(dat)
  inc <- income[,.SD,.SDcols=incomevars]
  data.table::setnames(inc,c("ID","yearinc","income"))
  inc <- inc[!is.na(income)]
  # Merge
  setkeyv(dat,c("ID","year"))
  dat[,num:=paste0("VVV",1:.N),by="ID"]
  dat <- dcast(dat,ID~num, value.var = "year")
  out <- merge(inc,dat,by="ID")
  out <- melt(out,measure.vars=names(out)[grepl("^VVV",names(out))],value.name="year")
  out[,variable:=NULL]
  out <- out[!is.na(year)]
  out <- out[yearinc<year & yearinc>=year-numyears] # relevant interval
  setkeyv(out,c("ID","yearinc","year")) 
  out <- out[,.SD[1],by=c("ID","yearinc","year")] # Max one record per year to compensate for quartiles
  out[,income:=as.numeric(income)]
  out <- out[,.(income=mean(income,na.rm=TRUE)),by=c("ID","year")]
  setnames(out,"ID",datvars[1])
  out
}

