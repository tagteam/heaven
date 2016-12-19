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
  
  for (jj in j)
    dpp$drugs[[jj]]$period <- value
  
  return(dpp)
}  



