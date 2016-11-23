##' Update database
##'
##' Update database
##' @title Update database
##' @param dpp a 
##' @param id a
##' @param atc a
##' @param pdate a 
##' @param strength a
##' @param npack a
##' @param ppp a
##' @param add a 
##' @param value a
##' @author Helene Charlotte Rytgaard
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
  
  if (inherits(newdata$pdate, "factor"))
    newdata$pdate <- as.Date(newdata$pdate)
  
  if (add) {
    dpp$drugdb <- rbind(dpp$drugdb, newdata)
  } else {
    dpp$drugdb <- newdata
  }

  dpp$drugdb = dpp$drugdb[order(dpp$drugdb$id, dpp$drugdb$pdate), ]
  
  return(dpp)
}

