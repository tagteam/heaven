##' Update database
##'
##' Update database
##' @title Update drug prescription database
##' @param dpp Data preprocessing object
##' @param id The name of the column containing the id numbers
##'     (default is pnr)
##' @param atc The name of the column containing the atc codes
##'     (default is atc)
##' @param pdate The name of the column containing the prescription
##'     dates (default is eksd)
##' @param strength The name of the column containing the numerical
##'     dose strengths (default is strnum)
##' @param npack The name of the column containing the number of
##'     packages (default is apk)
##' @param ppp The name of the column containing the number of pills
##'     per package (default is packsize)
##' @param add Logical value, per default set to FALSE which means
##'     that already existing data in the preprocessing object will be
##'     overwritten.
##' @param value The data set to be loaded to the object.
##' @examples
##' 
##' drugdata <- simPrescriptionData(10)
##' 
##' d <- dpp()
##' drugdb(d) <- drugdata
##' 
##' @author Helene Charlotte Rytgaard
##' @export
`drugdb<-` <- function(dpp,
                       id       = "pnr",
                       atc      = "atc",
                       pdate    = "eksd",
                       strength = "strnum",
                       npack    = "apk",
                       ppp      = "packsize",
                       add = FALSE,
                       value) {
    varnames <- c(id,atc,pdate,strength,npack,ppp)
    varTF <- sapply(varnames, function(x) {
        any(names(value) == x)
    })
    
    if (any(!varTF)) {
        cat("ERROR - the following specified columns cannot be found in data:")
        cat("\n")
        cat(paste(varnames[!varTF], collapse=", "))
        cat("\n")
        cat("\n")
        cat('see help("drugdb<-") for more details')
    } else {  
        newdata <- subset(value, select = varnames)
        # Could the following be a bug if the order of the columns are not as expected?
        setnames(newdata,c("id", "atc", "pdate", "strength", "npack", "ppp")) 
        if (!inherits(newdata$pdate, "Date")) {
            cat("ERROR: Dates must be in date format. Use as.Date()")
        } else {
            dpp$drugdb <- newdata
        }
        setkeyv(dpp$drugdb,c("id","pdate"))
        return(dpp)
    }
    ## }
}

