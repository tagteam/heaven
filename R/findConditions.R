### findCondition.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Mar 11 2019 (10:30) 
## Version: 
## Last-Updated: Mar 11 2019 (10:58) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:    
##' Find conditions 
##'
##' Find conditions
##' @title Find conditions in registry data
##' @param data Data in which to search for conditions 
##' @param vars Name(s) of variable(s) in which to search
##' @param keep a character vector of the columns in Data.table to keep in output
##' @param conditions A named list of (vectors of) search strings. See examples.
##' @param match A variable to tell how to use the character vectors: "exact"=exactly matches the search string, "contains"=contains the search string, "starts"=Starts with the search string, "end"=Ends with the search string
##' @param replace if replace=NULL then the set of variables corresponding to the names "conditions" will be inter 0/1 indicating whether the condition was met.  If replace is the name of a variable in Data, then that variable will be in the output replacing "1".
##' @return
##'  A data table that includes the "keep-variables" and a set of ivariables corresponding to the names of the vectors in the list.  For each record these variables takes on values 0/1 when the condition is not-met/met OR NA/value-of-Replace-variable if replace is NULL/name-of-variable
##' @examples
##' \dontrun{
##' library(data.table)
##' library(heaven)
##' d=simPrescriptionData(10000)
##' findCondition(data=d,vars="atc",conditions=list(drug1=c("N07","A12"),drug2=c("R03","R05")),match="start")
##' findCondition(data=d,vars="atc",conditions=list(drug1=c("N07","A12"),drug2=c("R03","R05")),match="start",combine="wide")
##' }
##' @export 
##' @author Christian Torp-Pedersen  <ctp@&hst.aau.dk>, Thomas A. Gerds <tag@@biostat.ku.dk>
findCondition <- function(data, id ="pnr", vars, keep, conditions, match="contain",replace=NULL,combine="long"){
    requireNamespace("data.table")
    setDT(data)
    setkeyv(data,id)
    out <- NULL
    for (cc in conditions){
        regexp <- switch(match,"exact"=paste0("^",cc,"$",collapse="|"),"contains"=paste0(cccollapse="|"),"start"=paste0("^",cc,collapse="|"))
        if (length(vars)==1) {
            found <- grepl(regexp,data[[vars]])
        }else{
            found <- rep(0L,NROW(data))
            for (v in vars){
                found <- found | grepl(regexp,data[[vars]])                
            }
        }
        if (length(out)==0){
            out <- data[found]
        } else{
            if (combine=="long"){
                out <- rbindlist(list(out,data[found]))
            }else{
                out <- out[data[found]]
            }
        }
    }
    out
}


######################################################################
### findCondition.R ends here
