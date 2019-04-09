### findCondition.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Mar 11 2019 (10:30) 
## Version: 
## Last-Updated: Apr  9 2019 (16:51) 
##           By: Thomas Alexander Gerds
##     Update #: 14
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:  
#' Find conditions
#'
#' Find conditions
#' @title Find conditions in registry data
#' @description
#' This functions is useful for the very common task of selecting cases based 
#' on a code which has complete or partial match to a vector of character vari-
#' ables.
#' 
#' The function is designed to search a group of variables (character) for 
#' multiple conditions defined in a list of named character vectors. The func-
#' tion will produce a data.table with selected variables for cases where a
#' match is found.
#' 
#' See examples for common use of the output
#' @usage
#' findCondition(data, vars, keep, conditions, match="contain",cond="cond")
#' 
#' @param data Data in which to search for conditions
#' @param vars Name(s) of variable(s) in which to search.
#' @param keep a character vector of the columns in Data.table to keep in
#' output
#' @param conditions A named list of (vectors of) search strings. See examples.
#' @param match A variable to tell how to use the character vectors:
#' "exact"=exactly matches the search string, "contains"=contains the search
#' string, "starts"=Starts with the search string, "end"=Ends with the search
#' string
#' @return
#' A data table that includes the "keep-variables" and a variable "cond" which
#' identifies the condition searched for
#' @examples
#' library(heaven)
#' library(data.table)
#' opr <- data.table(
#'   pnr=1:100,opr=paste0(rep(c('A','B'),50),seq(0,100,10)),
#'   oprtil=paste0(rep(c('A','C'),50),seq(0,100,10)),
#'   odto=101:200
#' )
#' search <- list(Cond1=c('A1','A2'),Cond2=c('B10','B40','B5'),
#' Cond3=c('A1','C20','B2'))
#'
#' out <- findCondition(opr,vars=c("opr","oprtil"),keep=c("pnr","odto"),
#' conditions=search,match="start",cond="x")
#' ### And how to use the result:
#' # Find first occurence of each condition and then use "dcast" to create
#' # a data.table with vectors corresponding to each condition.
#' test <- out[,list(min=min(odto)),by=c("pnr","cond")]
#' # provide a list of variables with one value each
#' test2 <- dcast(pnr~cond,data=test,value.var="min")
#' test2 # A datatable with first dates of each condition for each pnr, but only 
#'       # for pnr with at least one condition
#' # Define a condition as present when before a certain date
#' dates <- data.table (pnr=1:100,basedate=sample(0:200,size=100,replace=TRUE))       
#' test3 <- merge(out,dates,by="pnr")
#' test3[,before:=as.numeric(odto<=basedate)] # 1 when condition fulfilled
#' test3[is.na(before),before:=0] # change NA to zero
#' test4 <- dcast(pnr~cond,value.var="before",data=test3)
#' @export
#' @author Christian Torp-Pedersen  <ctp@heart.dk>, Thomas A. Gerds <ta
#' tag@@biostat.ku.dk>
findCondition <- function(data,
                          vars,
                          keep,
                          conditions,
                          match="contain",
                          variable.name="variable"){
    match <-match.arg(match,c("exact","contains","start"))
    if (!is.character(vars) | !is.character(keep)) stop ("Error -  vars or keep not character")  
    if (!class(conditions)=="list") stop ("Error - Conditions not a list")
    requireNamespace("data.table")
    setDT(data)
    data <-copy(data)
    for (variable in unique(c(vars,keep))){
        if (!variable %in% names(data)) stop(paste0("Error - ",variable," not in data to be analysed"))
    }
    out <- NULL
    for (i in 1:length(conditions)){
        cc <- conditions[[i]]
        name <- names(conditions)[[i]]
        regexp <- switch(match,"exact"=paste0("^",cc,"$",collapse="|"),"contains"=paste0(cc ,collapse="|"),"start"=paste0("^",cc,collapse="|"))
        if (length(vars)==1) {
            found <- grepl(regexp,data[[vars]])
        }else{
            found <- rep(0L,NROW(data))
            for (v in vars){
                found <- found | grepl(regexp,data[[v]])                
            }
        }
        if (length(out)==0){
            out <- data[found]
            out[,cond:=name]
        } else{
            temp <- data[found]
            temp[,cond:=name]
            out <- rbindlist(list(out,temp))
        }
    }
    out <- out[,c("cond",keep),with=FALSE]
    setnames(out,"cond",cond)
    out
}


######################################################################
### findCondition.R ends here
