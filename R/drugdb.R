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
`drugdb<-` <- function(dpp, formula = pnr ~ atc + eksd + strnum + apk + packsize, 
                       add = FALSE, value) {
  
  fstr <- deparse(formula)
 
  split1 <- gsub(" ", "", strsplit(fstr, "~")[[1]])
  split2 <- strsplit(split1[2], "\\+")[[1]]
  
  varnames <- c(strsplit(split1, "~")[[1]], split2)

  newdata        <- subset(value, select = varnames)
  names(newdata) <- c("pnr", "atc", "eksd", "strnum", "apk", "packsize")

  if (add) {
    dpp$drugdb <- rbind(dpp$drugdb, newdata)
  } else {
    dpp$drugdb <- newdata
  }

  dpp$drugdb = dpp$drugdb[order(dpp$drugdb$pnr), ]
  
  return(dpp)
} 

