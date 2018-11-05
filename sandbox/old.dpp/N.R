##' Set the number of prescriptions to use in calculations
##'
##' @title Prescription window
##' @param dpp The preprocessing object
##' @param drug If specified, the name (in quotes) of the drug to update the value for. If unspecified, the value is updated for all drugs in the preprocessing object. 
##' @param value The value for how many prescriptions to use back in time for calculations
##' 
##' @examples
##' 
##' d <- dpp()
##' drug(d, "treatment1") <- atc("A12B")
##' 
##' pwindow(d, drug="treatment1") <- 3
##' 
##' @author Helene Charlotte Rytgaard
##' @export
`prescriptionwindow<-` <- function(dpp, drug=NULL, value) {

  if (length(drug) > 0) {
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  } else 
    j <- 1:length(dpp$drugs)
  
  for (jj in j)
    dpp$drugs[[jj]]$prescriptionwindow <- value
  
#  dpp$N <- value
  
  return(dpp)
}  

