### drugdb.R ---
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
`admdb<-` <-
  function(x, ..., value) UseMethod("admdb<-")

##' @export
`admdb<-` <- function(dpp,
                      id     = pnr,
                      inddto = inddto,
                      uddto  = uddto, 
                      add = FALSE, value) {
  
    varnames <- c(deparse(substitute(id)),
                  deparse(substitute(inddto)),
                  deparse(substitute(uddto)))
  
  newdata        <- data.frame(sapply(1:length(varnames), function(i) value[, names(value) == varnames[i]])) 
  names(newdata) <- c("id", "inddto", "uddto")

  if (add) {
    dpp$admdb <- rbind(dpp$admdb, newdata)
  } else {
    dpp$admdb <- newdata
  }
  
  return(dpp)
}  

