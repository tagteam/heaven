### process.R ---
#----------------------------------------------------------------------
## author: Helene Charlotte Rytgaard
## created: October 14 2016
## Version:
## last-updated: 
##           By: 
##     Update #: 
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
##' @title Update database
##' @param ...
##' @author Helene Charlotte Rytgaard
##' @export
`process` <- function(dpp, id = NULL) {
  
  dpp1 <- preprocess(dpp, id = id)
  
  dpp2 <- lapply(1:length(dpp1), function(p) {

    idunique  <- unique(dpp1[[p]]$id)
    doses     <- dpp$drugs[[p]]$doses
    treatname <- names(dpp1)[p]
    
    dosesmissing <- !(dpp1[[p]]$strength %in% doses$value)
    
    baddata <- 0 
    
    if (any(dpp1[[p]]$pdate < 0)) {
      print(cat("Warning - non-valid prescription - negative date specified"))
      baddata <- 1
    } 
    if (any(dpp1[[p]]$npack < 0.0001)) {
      print(cat("Warning - non-valid prescription - number of packages not valid"))
      baddata <- 1
    } 
    if (any(dpp1[[p]]$ppp < 0.5)) {
      print(cat("Warning - non-valid prescription - pills per package not valid"))
      baddata <- 1
    } 
    if (any(dosesmissing)) {
      print(cat("Warning - not all doses are defined for treatment named", treatname))
      print(cat("Missing:", paste(unique(dpp1[[p]]$strength[dosesmissing]), collapse=", ")))
      baddata <- 1
    }
    
    if (baddata)
      print(cat("Computations for treatment named", treatname, "will terminate", "\n"))
    else 
      print(cat(" ", "\n"))
    
    if (length(doses) > 0) 
      do.call("rbind", lapply(1:length(idunique), function(i) {
        
        dat    <- dpp1[[p]][dpp1[[p]]$id == idunique[i], ]
        admdat <- dpp$admdb[dpp$admdb$id == idunique[i], ]
        
        if (dim(dat)[1] > 0)
          innerprocess(dat, admdat, doses, treatname, dpp$N) 
      }))
    
  })
  
  names(dpp2) <- names(dpp1)
  
  return(dpp2)
} 

