##' Set the relevant period of time. 
##' 
##' @title Set period of time
##' @param dpp The preprocessing object
##' @param drug If specified, the name (in quotes) of the drug to update the value for. If unspecified, the value is updated for all drugs in the preprocessing object. 
##' @param value Date vector of start and end date (must be date format).
##' 
##' @examples
##' 
##' d <- dpp()
##' drug(d, "treatment1") <- atc("A12B")
##' 
##' period(d) <- as.Date(c("1997-01-01", "2012-12-31"))
##' 
##' @author Helene Charlotte Rytgaard
##' @export
`period<-` <- function(dpp, drug=NULL, value) {
  
  if (!inherits(value, "Date")) {
    cat("ERROR: Must be in date format. Use as.Date()")
  } else {
    if (length(drug) > 0) {
      j <- (1:length(dpp$drugs))[names(dpp$drugs) == drug]
    } else 
      j <- 1:length(dpp$drugs)
    
    for (jj in j)
      dpp$drugs[[jj]]$period <- value
    
    return(dpp)
  }
}  



