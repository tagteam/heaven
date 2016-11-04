##' Create data preprocessing object 
##'
##' Create data preprocessing object (dpp)
##' @title Create data preprocessing object (dpp)
##' @author Helene Charlotte Rytgaard
##' @export
dpp <- function() { 
  out = structure(list(drugdb = NULL,
                       admdb  = NULL, 
                       drugs  = NULL, 
                       period = c(as.Date("1994-01-01"), as.Date(Sys.Date()+5000)),
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
    cat("with corresponding dose values:", "\n")
    print(do.call("rbind", lapply(doses, function(x) as.data.frame(x))))
  } 
  cat("\n")
  if (length(dpp$N) > 0)
    cat("Using N =", dpp$N, "prescriptions back in time", "\n")
  if (length(dpp$period) > 1)
    cat("Only interested in prescriptions between", as.character(dpp$period[1]), "and", as.character(dpp$period[2]))  
  cat("\n")
} 


