#' @title matchReport - report of matching from incidenceMatch and exposureMatch
#' 
#' @description  
#' The function provides very simple tables of the success of finding controls and
#' the reuse of cases and controls. Designed to deal with the results of function
#' riskSetMatch
#' @param x result of exposureMatch or incidenceMatch
#' @author Christian Torp-Pedersen & Thomas Alexander Gerds
#' @details 
#' This function can be helpful to reevaluate matching options after exact matching.
#' If many
#' cases do not find controls it may be desirable to reduce the number of
#' matching terms or to do 
#' further rounding of continuous matching variables.
#' @seealso incidenceMatch exposureMatch
#' @export
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15)) 
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' caseIndex <- c(seq(1,40,1),seq(5,47,3))
#' controlIndex <- caseIndex
#' library(data.table)
#' dat <- data.table(ptid,case,sex,byear,caseIndex,controlIndex)
#' # Very simple match without reuse - no dates to control for
#' matched.data <- incidenceMatch(ptid="ptid",event="case",terms=c("byear","sex"),
#' data=dat,n.controls=2,
#' case.index="caseIndex",end.followup="controlIndex")
#' matchReport(matched.data)
matchReport <- function(x){
    .SD=.N=N=caseid=NULL
    id=attr(x,"id")
    event=attr(x,"event")
    cat("\n","------------------------------------------------------------------","\n","Matching success")
    ## Matching success:
    if (!("n.controls" %in% names(x)))
        n.controls <- as.vector(x[,(.N-1),by="case.id"])
    else
        n.controls <- x[["n.controls"]]
    cat("\n", "Line 1, number of controls found - Line 2, number of occurrences:\n")
    print(table(n.controls))
    cat("\n","------------------------------------------------------------------")
    ##Reuse of controls
    n.reuse<-as.vector(x[x[[event]]==0,.N,by=id])[["N"]]
    cat("\n","Reuse/use of controls","\n","Line 1, number of times - Line 2, number of occurrences","\n")
    print(table(n.reuse))
    cat("\n","------------------------------------------------------------------\n")
}

 
 
