##' Processing function, to perform calculations according to the data and variables specified in preprocessing object. 
##' 
##' @title Process to estimate prescription lengths and averages 
##' @param dpp Data preprocessing object
##' @param treatments If specified, calcations will only be performed for these treatments. 
##' @param id If specified, calculations will only be performed for these individuals.
##' @param trace If TRUE, messages are outputted for the user. 
##' @param out_data If TRUE, all data is outputted, and this can be used for plotting. This should only be used for debugging or similar. 
##' @param keep_data If TRUE, the input data is saved in the object as well. This can can be used for plotting. 
##' @author Helene Charlotte Rytgaard
##' @export
process <- function(dpp, treatments = NULL, id = NULL, trace = FALSE, out_data = TRUE, keep_data = FALSE, FUN = innerprocess) {
  
  #  drugdb <- preprocess(dpp, id = id, trace = trace)
  
  if (length(treatments) == 0) 
    treatments <- names(dpp$drugs)
  
  ##--- unique id's
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
      baddata <- 1
    if (length(period) == 0)
      baddata <- 1
    if (length(N) == 0)
      baddata <- 1
    
    ##--- preprocesssing ---##
    if (length(id) > 0) {
      ivec <- (dpp$drugdb$id %in% id &
        dpp$drugdb$pdate <= period[2] & dpp$drugdb$pdate >= period[1] & 
        dpp$drugdb$atc %in% atcs)
    } else {
      ivec <- dpp$drugdb$pdate <= period[2] & dpp$drugdb$pdate >= period[1] & 
        dpp$drugdb$atc %in% atcs
    }
    
    dosesmissing <- !(dpp$drugdb$strength[ivec] %in% doses$value)
    
    baddata <- 0 
    
    if (any(dpp$drugdb$pdate[ivec] < 0)) {
      if (trace) print(cat("Warning - non-valid prescription - negative date specified"))
      baddata <- 1
    } 
    if (any(dpp$drugdb$npack[ivec] < 0.0001)) {
      if (trace) print(cat("Warning - non-valid prescription - number of packages not valid"))
      baddata <- 1
    } 
    if (any(dpp$drugdb$ppp[ivec] < 0.5)) {
      if (trace) print(cat("Warning - non-valid prescription - pills per package not valid"))
      baddata <- 1
    } 
    if (any(dpp$dosesmissing[ivec])) {
      if (trace) print(cat("Warning - not all doses are defined for treatment named", treatname))
      if (trace) print(cat("Missing:", paste(unique(drugdb$strength[dosesmissing]), collapse=", ")))
      baddata <- 1
    }
    
    if (baddata) {
      
      print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
      
    } else if (length(doses) > 0) {
      
      out <- do.call("rbind", lapply(1:length(idunique), function(i) {
        dat    <- dpp$drugdb[dpp$drugdb$id == idunique[i] & ivec, ]
        admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
        dat <- dat[order(dat$pdate), ]
        if (dim(dat)[1] > 0)
          FUN(dat, admdat, doses, treatname, N, maxdepot, trace, out_data) 
      }))
      
      if (keep_data) {
        attr(out, "drugdb") <- dpp$drugdb[ivec]
      }
      
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
    drugdb  <- lapply(out, function(x) attr(x, "drugdb"))
    names(drugdb) <- treatments
    #  treat <- t(data.frame(lapply(1:length(d$drugs), function(i) 
    #    sapply(d$drugs[[i]]$atc, function(x)
    #      c(x, names(d$drugs)[i])))))
    #  drugdb$treatment <- sapply(drugdb$atc, function(x) treat[treat[, 1] == x, 2])
    #  attr(outlist, "drugdb") <- drugdb
    outlist$drugdb <- drugdb
  }
  
  return(outlist)
}


