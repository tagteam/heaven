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
                      add = FALSE,
                      value) {
    varnames <- c(deparse(substitute(id)),
                  deparse(substitute(inddto)),
                  deparse(substitute(uddto)))
    if (add) {
        dpp$admdb <- rbind(dpp$admdb, value)
    } else {
        dpp$admdb <- value
    }
    return(dpp)
}  

