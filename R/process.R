##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param id a 
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @examples
##' data(samplePPIData)
##' 
##' @export
process <- function(dpp, treatments = NULL, id = NULL, trace = FALSE, out_data = TRUE, keep_data = FALSE) {
  
  #  dpp1 <- preprocess(dpp, id = id, trace = trace)
    
    if (length(treatments) == 0) 
      treatments <- names(dpp$drugs)
    
    ##--- relevant id's
    if (length(id) == 0) {
      idunique <- unique(dpp$drugdb$id)
    } else 
      idunique <- unique(id[id %in% dpp$drugdb$id])
    
    treatfun <- function(treatname) {

      j            <- (1:length(dpp$drugs))[names(dpp$drugs) == treatname]
      atcs         <- unlist(dpp$drugs[[j]]$atc)
      doses        <- dpp$drugs[[j]]$doses
      maxdepot     <- dpp$drugs[[j]]$maxdepot
      period       <- dpp$drugs[[j]]$period
      N            <- dpp$drugs[[j]]$N
      
      if (length(maxdepot) == 0)
        maxdepot = 10
      if (length(period) == 0)
        period   = c(1, 1e10)
      if (length(N) == 0)
        N       = 2
      
      ##--- preprocesssing ---##
      if (length(id) > 0) {
        drugdb <- dpp$drugdb[id %in% id, ]
        admdb  <- dpp$admdb[id %in% id, ]
      } else {
        drugdb <- dpp$drugdb
        admdb  <- dpp$admdb
      }
      
      ##--- only look at relevant dates 
      drugdb <- drugdb[drugdb$pdate <= period[2] & drugdb$pdate >= period[1], ]
      dpp1   <- drugdb[drugdb$atc %in% atcs, ]

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
      
      if (baddata) {
        print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
      } else if (length(doses) > 0) {
        out <- do.call("rbind", lapply(1:length(idunique), function(i) {
          dat    <- dpp1[dpp1$id == idunique[i], ]
          admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
          dat <- dat[order(dat$pdate), ]
          if (dim(dat)[1] > 0)
            innerprocess(dat, admdat, doses, treatname, N, maxdepot, trace, out_data) 
        }))
        if (keep_data)
          attr(out, "drugdb") <- dpp1
        attr(out, "period") <- period
        return(out)
      }
    }
    
    out <- lapply(treatments, treatfun)

    outlist <- structure(out, 
                         out_data = out_data,
                         period   = lapply(out, function(x) attr(x, "period")),
                         class    = "dppout")
    names(outlist) <- treatments

    if (keep_data) {
      dpp1  <- lapply(out, function(x) attr(x, "drugdb"))
      names(dpp1) <- treatments
    #  treat <- t(data.frame(lapply(1:length(d$drugs), function(i) 
    #    sapply(d$drugs[[i]]$atc, function(x)
    #      c(x, names(d$drugs)[i])))))
    #  dpp1$treatment <- sapply(dpp1$atc, function(x) treat[treat[, 1] == x, 2])
    #  attr(outlist, "drugdb") <- dpp1
      outlist$drugdb <- dpp1
    }
    
    return(outlist)
}


