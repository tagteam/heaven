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
                       period = c(1, 1e10),
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
    cat("Dose values to be used in calculations:", "\n")
    print(do.call("rbind", lapply(doses, function(x) as.data.frame(x))))
    cat("\n")
    if (length(dpp$drugdb) > 0) {
      dosesindata  = sapply(drugs, function(x) unique(dpp$drugdb$strength[dpp$drugdb$atc %in% x]))
      dosesmissing = sapply(1:length(dosesindata), function(i) dosesindata[[i]][!(dosesindata[[i]] %in% sapply(doses, function(x)
        x$value)[[i]])])
      cat("Doses found in data and not in the above:", "\n", sapply(1:length(drugs), function(i)
        paste(paste0(names(drugs)[i], ":"),
        sapply(dosesmissing[i], function(x) paste(x, collapse = ", ")), "\n")), "\n")
    }
    cat("\n")
  } 
  if (length(dpp$N) > 0)
    cat("Using N =", dpp$N, "prescriptions back in time", "\n")
  if (length(dpp$period) > 1 & dpp$period[1] != 1)
    cat("Only interested in prescriptions between", as.character(dpp$period[1]), "and", as.character(dpp$period[2]))  

  cat("\n")
} 
