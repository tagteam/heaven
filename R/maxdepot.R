##' maximum repositary
##'
##' maximum repositary
##' @title Update database
##' @param dpp a
##' @param ... a 
##' @param value a
##' @author Helene Charlotte Rytgaard
##' @export
`maxdepot<-` <- function(dpp, drug=NULL, value) {

  if (length(drug) > 0) {
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  } else 
    j <- 1:length(dpp$drugs)
  
  for (j in 1:length(j)) 
    dpp$drugs[[j]]$maxdepot <- value
  
#  dpp$maxdepot <- value
  
  return(dpp)
}  
