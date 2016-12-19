##' Set the number of prescriptions to use in calculations
##'
##' @title Prescription window
##' @param dpp a
##' @param ... a 
##' @param value a
##' @author Helene Charlotte Rytgaard
##' @export
`pwindow<-` <- function(dpp, drug=NULL, value) {

  if (length(drug) > 0) {
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  } else 
    j <- 1:length(dpp$drugs)
  
  for (jj in j)
    dpp$drugs[[jj]]$N <- value
  
#  dpp$N <- value
  
  return(dpp)
}  

