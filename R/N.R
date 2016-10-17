### N.R ---
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
`N<-` <-
  function(x, ..., value) UseMethod("N<-")

##' @export
`N<-` <- function(dpp, ..., value) {

  dpp$N <- value
  
  return(dpp)
}  

