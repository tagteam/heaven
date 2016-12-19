##' The maximum depot that can be transferred from one prescription period to the next.
##'
##' @title Set value of maximum depot. 
##' @param dpp The preprocessing object
##' @param drug If specified, the name (in quotes) of the drug to update the value for. If unspecified, the value is updated for all drugs in the preprocessing object. 
##' @param value The maximum depot that can be transferred from one prescription period to the next.
##' 
##' @examples
##' 
##' d <- dpp()
##' drug(d, "treatment1") <- atc("A12B")
##' 
##' maxdepot(d, drug="treatment1") <- 1000
##' 
##' @author Helene Charlotte Rytgaard
##' @export
`maxdepot<-` <- function(dpp, drug=NULL, value) {

  if (length(drug) > 0) {
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  } else 
    j <- 1:length(dpp$drugs)
  
  for (jj in j)
    dpp$drugs[[jj]]$maxdepot <- value
  
#  dpp$maxdepot <- value
  
  return(dpp)
}  
