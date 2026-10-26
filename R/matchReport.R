#' @title matchReport - report of matching from incidenceMatch and exposureMatch
#' 
#' @description  
#' The function provides very simple tables of the success of finding controls and
#' the reuse of cases and controls. Designed to deal with the results of function
#' riskSetMatch
#' @usage matchReport(x, verbose = TRUE)
#' @param x result of exposureMatch or incidenceMatch
#' @param verbose Logical. Print the report when \code{TRUE}; set to
#' \code{FALSE} to retrieve the tables without console output.
#' @return Invisibly returns a named list of two objects of class \code{table}.
#' \code{matching.success} counts cases by the number of matched controls;
#' \code{control.reuse} counts control subjects by the number of times each
#' was selected. The report is also printed when \code{verbose = TRUE}.
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
matchReport <- function(x, verbose = TRUE){
  .N <- NULL
  id <- attr(x, "id")
  event <- attr(x, "event")
  n.controls <- x[, .N - 1L, by = "case.id"][["V1"]]
  n.reuse <- x[x[[event]] == 0, .N, by = id][["N"]]
  report <- list(matching.success = table(n.controls),
                 control.reuse = table(n.reuse))
  if (verbose) {
    cat("\nMatching success\nNumber of controls found / number of cases:\n")
    print(report$matching.success)
    cat("\nReuse/use of controls\nNumber of times selected / number of subjects:\n")
    print(report$control.reuse)
  }
  invisible(report)
}



