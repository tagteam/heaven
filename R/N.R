##' Set the number of prescriptions to use in calculations
##'
##' @title Prescription window
##' @param dpp a
##' @param ... a 
##' @param value a
##' @author Helene Charlotte Rytgaard
##' @export
`pwindow<-` <- function(dpp, ..., value) {

  dpp$N <- value
  
  return(dpp)
}  
