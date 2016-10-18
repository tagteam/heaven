### process.R ---
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
##' @title Update database
##' @param ...
##' @author Helene Charlotte Rytgaard
##' @export
`process` <-
  function(x, ..., value) UseMethod("drugdb")

##' @export
`process` <- function(dpp, id = NULL, value) {
  
  if (length(id) > 0) {
    drugdb = lapply(dpp$drugdb, function(x) x[x$pnr %in% id, ]) 
    admdb  = lapply(dpp$admdb, function(x) x[x$pnr %in% id, ]) 
  } else {
    drugdb = dpp$drugdb
    admdb  = dpp$admdb
  }
  
  
  

  return(drugdb)
} 



