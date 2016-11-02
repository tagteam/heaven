##' Update date database
##' 
##' Update database
##' @title Update database
##' @param dpp a 
##' @param id a
##' @param inddto a
##' @param uddto a
##' @param add a
##' @param value a
##' @author Helene Charlotte Rytgaard
##' @export  
`admdb<-` <- function(dpp,
                      id     = pnr,
                      inddto = inddto,
                      uddto  = uddto, 
                      add = FALSE, value) {
  
    varnames <- c(deparse(substitute(id)),
                  deparse(substitute(inddto)),
                  deparse(substitute(uddto)))
  
  newdata        <- data.frame(id     = value[, names(value) == varnames[1]], 
                               inddto = as.Date(value[, names(value) == varnames[2]]),
                               uddto  = as.Date(value[, names(value) == varnames[3]]))

  if (add) {
    dpp$admdb <- rbind(dpp$admdb, newdata)
  } else {
    dpp$admdb <- newdata
  }
  
  return(dpp)
}  

