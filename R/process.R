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
process <- function(dpp, treatments = NULL, id = NULL,maxdepot = 10,trace = FALSE) {
    
    period <- dpp$period
    dpp1 <- preprocess(dpp, id = id, trace = trace)
    
    if (length(treatments) == 0) 
      treatments <- names(dpp$drugs)
      
    idunique     <- unique(dpp1$id)

    treatfun <- function(treatname) {

      j            <- (1:length(dpp$drugs))[names(dpp$drugs) == treatname]
      doses        <- dpp$drugs[[j]]$doses 
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
      if (baddata)
        print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
      else if (length(doses) > 0) {
        out <- do.call("rbind", lapply(1:length(idunique), function(i) {
          dat    <- dpp1[dpp1$id == idunique[i], ]
          admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
          dat <- dat[order(dat$pdate), ]
          if (dim(dat)[1] > 0)
            innerprocess(dat, admdat, doses, treatname, dpp$N, maxdepot, trace) 
        }))
        return(out)
      }
    }
    ## })
    ## names(dpp2) <- names(dpp1)
    ## return(dpp2)
    
    outlist <- sapply(treatments, treatfun)
    names(outlist) <- treatments
    
    return(outlist)
}

