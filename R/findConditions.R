### findCondition.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds & Christian Torp-Pedersen
## Created: Mar 11 2019 (10:30) 
## Version: 
## Last-Updated: Nov 15 2019 (07:43) 
##           By: Thomas Alexander Gerds
##     Update #: 31
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
# Core moved to c++
#----------------------------------------------------------------------
## 
### Code:  
#' @title Find conditions in registry data
#' @description
#' This functions is useful for the very common task of selecting cases based 
#' on a code which has complete or partial match to a vector of character vari-
#' ables.
#' 
#' The function is designed to search a group of variables (character) for 
#' multiple conditions defined in a list of named character vectors. The func-
#' tion will produce a data.table with selected variables for cases where a
#' match is found.  In addition a list of names character vectors can have
#' exclusions from the search.  This last facility is useful if e.g. all
#' cancer except non melanoma skin cancer is sought.  In that case inclusion 
#' can have all cancers and the exclusions just the non-melanoma skin cancer.
#' 
#' See examples for common use of the output
#' @usage
#' findCondition(data,ptid="pnr", vars, keep, conditions, exclusions=NULL, 
#' match="contain",condition.name="X")
#' 
#' @param data Data in which to search for conditions
#' @param vars Name(s) of variable(s) in which to search.
#' @param keep a character vector of the columns in Data.table to keep in
#' output
#' @param conditions A named list of (vectors of) search strings. See examples.
#' @param exclusions A names list of (vectors of) search strings to exclude
#' from the output.
#' @param match A variable to tell how to use the character vectors:
#' "exact"=exactly matches the search string, "contains"=contains the search
#' string, "starts"=Starts with the search string, "end"=Ends with the search
#' string
#' @param condition.name Name of variable(s) where values define conditions. 
#' The values of this variable are the names from parameter "conditions".
#' @return
#' A data table that includes the "keep-variables" and a variable named 
#' \code{condition.name} which #' identifies the condition searched for
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
#' excl <- list(Cond2='B100')
#'
#' out <- findCondition(opr,vars=c("opr","oprtil"),keep=c("pnr","odto"),
#'         conditions=search, exclusions=excl,
#'         match="start",condition.name="cond")
#' ### And how to use the result:
#' # Find first occurence of each condition and then use "dcast" to create
#' # a data.table with vectors corresponding to each condition.
#' test <- out[,list(min=min(odto)),by=c("pnr","cond")]
#' # provide a list of variables with one value each
#' test2 <- dcast(pnr~cond,data=test,value.var="min")
#' test2 # A datatable with first dates of each condition for each pnr, but only 
#'       # for pnr with at least one condition
#' # Define a condition as present when before a certain index date
#' dates <- data.table (pnr=1:100,basedate=sample(0:200,size=100,replace=TRUE))       
#' test3 <- merge(out,dates,by="pnr")
#' test3[,before:=as.numeric(odto<=basedate)] # 1 when condition fulfilled
#' test4 <- dcast(pnr~cond,value.var="before",data=test3)
#' # Whether to convert NAs to zero depends on the situation
#' @author Christian Torp-Pedersen  <ctp@heart.dk>, Thomas A. Gerds <tag@biostat.ku.dk>
#' @export
findCondition <- function (data,ptid="pnr", vars, keep, conditions,exclusions=NULL, match = "contain", 
          condition.name = "X") 
{
  .SD=pnrnum=.N=searchcol=NULL
  cond = NULL
  if (match=="start") match.num <- 0L
   else if (match=="exact") match.num <- 1L
   else if (match=="end") match.num <- 2L
   else if (match=="contain") match.num <- 3L
   else stop("Choise of match not appropriate")
  if (!is.character(vars) | !is.character(keep)) 
    stop("Error -  vars or keep not character")
  if (!class(conditions) == "list" | is.null(names(conditions))) 
    stop("Error - Conditions not a named list")
  if(!is.null(exclusions) & (!class(conditions) == "list" | is.null(names(conditions))))
    stop("Error - Exclusions should be NULL or a named list") 
  requireNamespace("data.table")
  setDT(data)
  conditions <- copy(conditions)
  exclusions <- copy(exclusions)
  for (variable in unique(c(vars, keep))) {
    if (!variable %in% names(data)) 
      stop(paste0("Error - ", variable, " not in data to be analysed"))
  }
  #vector of conditions and exclusions - all given the maximal length of any criteria and padding with blanks 
  condnames <- names(conditions) 
  num.cond <- length(condnames)
  max.cond <- max(sapply(conditions,length))
  conditions <- lapply(conditions,function(x){c(x,rep("",max.cond-length(x)))}) # All max length, padded with ""
  condnames <- lapply(condnames,function(x){rep(x,max.cond)}) #condnames aligned with conditions
  conditions <- unlist(conditions)
  condnames <- unlist(condnames)

  if(!is.null(exclusions)){
    exclnames <- names(exclusions)
    num.excl <- length(exclnames)
    max.excl <- max(sapply(exclusions,length))
    exclusions <- lapply(exclusions,function(x){c(x,rep("",max.excl-length(x)))})
    exclnames <- lapply(exclnames,function(x){rep(x,max.excl)}) #exclnames aligned with exclusions
    exclusions <- unlist(exclusions)
    exclnames <- unlist(exclnames)
  }
  else{
    exclusions <- ""
    exclnames <- ""
    max.excl <- 0L
    num.excl <- 0L
  }
 
  # Columnes to keep
  keepCols <- data[,.SD,.SDcols=keep]
  keepCols[,pnrnum:=1:.N]
  # Columns to search
  searchCols <- data[,.SD,.SDcols=c(vars)]
  searchCols[,pnrnum:=1:.N]
  # Make a single vector of searchCols
  if(length(vars)==1) setnames(searchCols,vars,"searchcol")
  else{
    searchCols <- data.table::rbindlist(lapply(1:length(vars),function(x){
      cols2 <- searchCols[,.SD,.SDcols=c("pnrnum",vars[x])]
      setnames(cols2,vars[x],"searchcol")
      cols2
      }
    ))
  } 
  #Remove unnecessary repetitions and blanks
  searchCols <- searchCols[!(searchcol=="")]
  setkeyv(searchCols,c("pnrnum","searchcol"))
  searchCols <- unique(searchCols)
  setcolorder(searchCols,c("pnrnum","searchcol"))
  #cpp
  out <- vectorSearch(searchCols[[1]],        # Vector of row numbers for searchCols
                     searchCols[[2]],        # Vector with search values
                     conditions,             # Vector of inclusion conditions
                     exclusions,             # Vector of exclusion conditions
                     condnames,              # Names of conditions - same length as conditions
                     exclnames,              # Names of exclusions - same length as exclusions
                     num.cond,               # Number of inclusion criteria 
                     max.cond,                # Number of inclusions in each block
                     num.excl,               # Number of of exclusion criteria 
                     max.excl,               # Number of exclusion criteria
                     length(searchCols[[1]]),# Length of searchCols vector
                     match.num)              # 0=start 1=exact 2=end 3=contain
  
  setDT(out)
  setnames(out,"X",condition.name)
  out <- merge(out,keepCols,by="pnrnum")
  out[,pnrnum:=NULL]
  out
}




