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
    
    newdata        <- subset(value, select = varnames)
    names(newdata) <- c("id", "inddto", "uddto")
    
    if (!inherits(newdata$pdate, "Date"))
      stop("dates must be in Date format")
    
    if (add) {
        dpp$admdb <- rbind(dpp$admdb, newdata)
    } else {
        dpp$admdb <- newdata
    }
    
    dpp$admdb = dpp$admdb[order(dpp$admdb$id), ]
    
    return(dpp)
}  

