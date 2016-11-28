##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param id a 
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
process <- function(dpp, treatments = NULL, id = NULL, trace = FALSE, out_data = TRUE, keep_data = FALSE) {
    
    maxdepot <- dpp$maxdepot
    period <- dpp$period
    dpp1 <- preprocess(dpp, id = id, trace = trace)
    
    if (length(treatments) == 0) 
      treatments <- names(dpp$drugs)
      
    if (length(id) == 0) {
      idunique <- unique(dpp1$id)
    } else 
      idunique <- unique(id[id %in% dpp1$id])

    treatfun <- function(treatname) {

      j            <- (1:length(dpp$drugs))[names(dpp$drugs) == treatname]
      doses        <- dpp$drugs[[j]]$doses 
      dpp2         <- dpp1[dpp1$atc %in% dpp$drugs[[j]]$atc, ]
      dosesmissing <- !(dpp2$strength %in% doses$value)
      
      baddata <- 0 
      
      if (any(dpp2$pdate < 0)) {
        if (trace) print(cat("Warning - non-valid prescription - negative date specified"))
        baddata <- 1
      } 
      if (any(dpp2$npack < 0.0001)) {
        if (trace) print(cat("Warning - non-valid prescription - number of packages not valid"))
        baddata <- 1
      } 
      if (any(dpp2$ppp < 0.5)) {
        if (trace) print(cat("Warning - non-valid prescription - pills per package not valid"))
        baddata <- 1
      } 
      if (any(dosesmissing)) {
        if (trace) print(cat("Warning - not all doses are defined for treatment named", treatname))
        if (trace) print(cat("Missing:", paste(unique(dpp2$strength[dosesmissing]), collapse=", ")))
        baddata <- 1
      }
      
      if (baddata) {
        print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
      } else if (length(doses) > 0) {
        out <- do.call("rbind", lapply(1:length(idunique), function(i) {
          dat    <- dpp2[dpp2$id == idunique[i], ]
          admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
          dat <- dat[order(dat$pdate), ]
          if (dim(dat)[1] > 0)
            innerprocess(dat, admdat, doses, treatname, dpp$N, maxdepot, trace, out_data) 
        }))
        return(out)
      }
    }
    
    outlist <- structure(lapply(treatments, treatfun), 
                         out_data = out_data,
                         period   = dpp$period,
                         class    = "dppout")
    names(outlist) <- treatments

    if (keep_data) {
      treat <- t(data.frame(lapply(1:length(d$drugs), function(i) 
        sapply(d$drugs[[i]]$atc, function(x)
          c(x, names(d$drugs)[i])))))
      dpp1$treatment <- sapply(dpp1$atc, function(x) treat[treat[, 1] == x, 2])
      attr(outlist, "drugdb") <- dpp1
    }
    
    return(outlist)
}


