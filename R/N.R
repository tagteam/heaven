##' Update database
##'
##' Update database
##' @title Update database
##' @param dpp a
##' @param ... a 
##' @param value a
##' @author Helene Charlotte Rytgaard
##' @export
`N<-` <- function(dpp, ..., value) {

  dpp$N <- value
  
  return(dpp)
}  
