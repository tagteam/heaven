
### drugdb.R ---
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
`drugdb<-` <-
  function(x, ..., value) UseMethod("drugdb<-")

##' @export
`drugdb<-` <- function(dpp,
                       id       = pnr,
                       atc      = atc,
                       pdate    = eksd,
                       strength = strnum,
                       npack    = apk,
                       ppp      = packsize,
                       add = FALSE, value) {
    
    varnames <- c(deparse(substitute(id)),
                  deparse(substitute(atc)),
                  deparse(substitute(pdate)),
                  deparse(substitute(strength)),
                  deparse(substitute(npack)), deparse(substitute(ppp)))

  newdata        <- subset(value, select = varnames)
  names(newdata) <- c("id", "atc", "pdate", "strength", "npack", "ppp")

  if (add) {
    dpp$drugdb <- rbind(dpp$drugdb, newdata)
  } else {
    dpp$drugdb <- newdata
  }

  dpp$drugdb = dpp$drugdb[order(dpp$drugdb$id), ]
  
  return(dpp)
}

