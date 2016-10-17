### dpp.R ---
#----------------------------------------------------------------------
## author: Helene Charlotte Rytgaard
## created: October 14 2016
## Version:
## last-updated: 
##           By: 
##     Update #: 
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
##' @title Create data preprocessing object (dpp)
##' @param ...
##' @author Helene Charlotte Rytgaard
##' @export
dpp <- function(...) { 
  out = structure(list(drugdb = NULL,
                       admdb  = NULL, 
                       drugs  = NULL, 
                       dosis  = NULL, 
                       period = NULL,
                       N      = 2),
                  class = "dpp")
  out
}

##' @export
print.dpp <- function(dpp) {
  cat("preprocessing object", "\n")
  cat("-----------------", "\n")
  cat("\n")
  if (length(dpp$drugs) > 0) {
    drugs = lapply(dpp$drugs, function(x) x$atc)
    cat("Calculations for treatment(s):", "\n", 
        paste(paste0(names(drugs), ":"),
              sapply(drugs, function(x) paste(x, collapse = ", ")), "\n"))
    cat("\n")
    doses = lapply(dpp$drugs, function(x) x$doses)
    cat("with corresponding dosis values:", "\n")
    print(do.call("rbind", lapply(doses, function(x) as.data.frame(x))))
  } 
  cat("\n")
  if (length(dpp$N) > 0)
    cat("Using N =", dpp$N, "prescriptions back in time", "\n")
  if (length(dpp$period) > 1)
    cat("Only interested in prescriptions between", 
        paste(sapply(d$period, function(x) paste(gsub("-", "/", x), collapse = "-")), collapse = " and "))  
  cat("\n")
} 



