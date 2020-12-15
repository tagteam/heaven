#' @title averageIncome
#' @description
#' This function calculates average income for a range of years prior to a date.
#' The function expects a data.table with yearly incomes from which to calcu-
#' late the average.
#' @usage averageIncome(data,income,datvars,incomevars,numyears=5)
#' @author Christian Torp-Pedersen 
#' @param data - A dataframe/table with with an identification variable and a
#' date from which the previous years' income should be averaged.
#' @param income - A dataframe with at least three variables which are the 
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
#' dat <- data.table(pnr=1:3, dato=as.Date(paste0(2018:2020,"-11-11")))
#' indkomst <- data.table(pnr=rep(1:3,5),year=rep(2015:2019,5),
#' income=rep(200:204,5))
#' averageIncome(dat,indkomst,c("pnr","dato"),c("pnr","year","income"))
#' @export
averageIncome <- function(data,income,datvars,incomevars,numyears=5){
  if(!"data.frame" %in% class(data)) stop("First variable must be a data.frame or data.table")
  if(!"data.frame" %in% class(income)) stop("First variable must be a data.frame or data.table")
  if(!class(datvars)=="character" | !length(datvars)==2) stop("datvars must be a character vector of two")
  if(!class(incomevars)=="character" | !length(incomevars)==3) stop("incomevars must be a character vector of three")
  year=yearinc=.SD= .=NULL 
  dat <- data[,.SD,.SDcols=datvars]
  data.table::setDT(dat)
  data.table::setnames(dat,c("ID","year"))
  dat[,year:=year(year)]
  inc <- income[,.SD,.SDcols=incomevars]
  data.table::setnames(inc,c("ID","yearinc","income"))
  # Merge
  out <- merge(inc,dat,by="ID")
  out <- out[yearinc<year & yearinc>=year-numyears] # relevant interval
  setkeyv(out,c("ID","yearinc"))
  out <- out[,.SD[1],by=c("ID","yearinc")] # Max one record per year to compensate for quartiles
  out[,income:=as.numeric(income)]
  out <- out[,.(income=mean(income,na.rm=TRUE)),by="ID"]
  setnames(out,"ID",datvars[1])
  out
}

