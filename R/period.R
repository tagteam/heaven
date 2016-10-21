### period.R ---
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
`period<-` <-
  function(x, ..., value) UseMethod("period<-")

##' @export
`period<-` <- function(dpp, value) {
  
  dpp$period <- value
  
  return(dpp)
}  

