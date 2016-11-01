##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param value a 
##' @author Helene Charlotte Rytgaard
##' @export
`period<-` <- function(dpp, value) {
  
  dpp$period <- value
  
  return(dpp)
}  

