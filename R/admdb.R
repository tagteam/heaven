##' Update date database
##' 
##' Update database
##' @title Update admission dates database
##' @param dpp Data preprocessing object
##' @param id The name of the column containing the id numbers
##'     (default is pnr)
##' @param inddto The name of the column containing the admission
##'     start date (default is inddto)
##' @param uddto The name of the column containing the admission end
##'     date (default is uddto)
##' @param add Logical value, per default set to FALSE which means
##'     that already existing data in the preprocessing object will be
##'     overwritten.
##' @param value The data set to be loaded to the object.
##' @examples
##' 
##' admdata <- simAdmissionData(10)
##' 
##' d <- dpp()
##' admdb(d) <- admdata
##' 
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
        newdata <- subset(value, select = varnames)
        setnames(newdata,c("id", "inddto", "uddto"))
      if (!inherits(newdata$inddto, "Date") | !inherits(newdata$uddto, "Date")) {
        cat("ERROR: Dates must be in date format. Use as.Date()")
      } else {
        if (add) {
          dpp$admdb <- rbindlist(list(dpp$admdb, newdata),use.names=TRUE)
        } else {
          dpp$admdb <- newdata
        }
        setkey(dpp$admdb,id,inddto,uddto)
        return(dpp)
      }
  }
}  

