##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param id a 
##' @param maxdepot a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
process <- function(dpp,id = NULL,maxdepot = 10,trace = FALSE) {
    period <- dpp$period
    dpp1 <- preprocess(dpp, id = id, trace = trace)
    ## dpp2 <- lapply(1:length(dpp1), function(p) {
    idunique     <- unique(dpp1$id)
    doses        <- dpp$drugs[[2]]$doses
    treatname    <- dpp$drugs[[1]][[1]] 
    dosesmissing <- !(dpp1$strength %in% doses$value)
    baddata <- 0 
    if (any(dpp1$pdate < 0)) {
        if (trace) print(cat("Warning - non-valid prescription - negative date specified"))
        baddata <- 1
    } 
    if (any(dpp1$npack < 0.0001)) {
        if (trace) print(cat("Warning - non-valid prescription - number of packages not valid"))
        baddata <- 1
    } 
    if (any(dpp1$ppp < 0.5)) {
        if (trace) print(cat("Warning - non-valid prescription - pills per package not valid"))
        baddata <- 1
    } 
    if (any(dosesmissing)) {
        if (trace) print(cat("Warning - not all doses are defined for treatment named", treatname))
        if (trace) print(cat("Missing:", paste(unique(dpp1$strength[dosesmissing]), collapse=", ")))
        baddata <- 1
    }
    ## if (baddata)
        ## print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
    if (length(doses) > 0)
        out <- do.call("rbind", lapply(1:length(idunique), function(i) {
            dat    <- dpp1[dpp1$id == idunique[i], ]
            admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
            dat <- dat[order(dat$pdate), ]
            if (dim(dat)[1] > 0)
                innerprocess(dat, admdat, doses, treatname, dpp$N, maxdepot, trace) 
        }))
    out
    ## })
    ## names(dpp2) <- names(dpp1)
    ## return(dpp2)
}

