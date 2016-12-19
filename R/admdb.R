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
    
    varTF <- sapply(varnames, function(x) {
      any(names(value) == x)
    })
    
    if (any(!varTF)) {
      cat("ERROR - the following specified columns cannot be found in data:")
      cat("\n")
      cat(paste(varnames[!varTF], collapse=", "))
      cat("\n")
      cat("\n")
      cat('see help("admdb<-") for more details')
    } else {
      newdata        <- subset(data.frame(value), select = varnames)
      names(newdata) <- c("id", "inddto", "uddto")
      
      if (!inherits(newdata$inddto, "Date") | !inherits(newdata$uddto, "Date")) {
        cat("ERROR: Dates must be in date format. Use as.Date()")
      } else {
        
        if (add) {
          dpp$admdb <- rbind(dpp$admdb, newdata)
        } else {
          dpp$admdb <- newdata
        }
        
        dpp$admdb = dpp$admdb[order(dpp$admdb$id), ]
        
        return(dpp)
      }
  }
}  

