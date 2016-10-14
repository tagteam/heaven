### updatedb.R ---
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
`updatedb` <-
  function(x, ...) UseMethod("updatedb")

`updatedb<-` <- function(dpp, var, add = FALSE, value) {
  
  varname <- deparse(substitute(var))
  
  i <- (1:length(dpp))[names(dpp) == varname]
  
  if (add) {
    dpp[[i]] <- list(dpp[[i]], value) 
  } else {
    dpp[[i]] <- value
  }
  
  names(dpp)[[i]] = varname
  
  return(dpp)
}  


