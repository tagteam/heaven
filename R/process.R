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

    idunique <- unique(dpp1[[p]]$id)
    
    doses     <- dpp$drugs[[p]]$doses
    
    if (length(doses) > 0) 
      do.call("c", lapply(1:length(idunique), function(i) {
        dat <- dpp1[[p]][dpp1[[p]]$id == idunique[i], ]
        if (dim(dat)[1] > 0)
          innerprocess(dat, doses, names(dpp1)[p]) 
      }))
    
  })
  
  return(dpp2)
} 

