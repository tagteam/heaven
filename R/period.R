##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param value a 
##' @author Helene Charlotte Rytgaard
##' @export
`period<-` <- function(dpp, drug=NULL, value) {
  
  if (length(drug) > 0) {
    j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
  } else 
    j <- 1:length(dpp$drugs)
  
  for (j in 1:length(j)) 
    dpp$drugs[[j]]$period <- value
  
  return(dpp)
}  



