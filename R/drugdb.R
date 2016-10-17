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

  newdata        <- data.frame(sapply(1:length(varnames), function(i) value[, names(value) == varnames[i]])) 
  names(newdata) <- c("pnr", "atc", "eksd", "strnum", "apk", "packsize")

  l <- length(dpp$drugdb)+1
  
  if (add) {
    dpp$drugdb[[l]] <- newdata
  } else {
    dpp$drugdb[[1]] <- newdata
  }

  return(dpp)
} 
