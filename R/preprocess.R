### preprocess.R ---
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
`preprocess` <- function(dpp, id = NULL) {
  
  ##--- only look at id=, if specified
  if (length(id) > 0) {
    drugdb <- dpp$drugdb[dpp$drugdb$pnr %in% id, ]
    admdb  <- dpp$admdb[dpp$admdb$pnr %in% id, ]
  } else {
    drugdb <- dpp$drugdb
    admdb  <- dpp$admdb
  }
  
  ##--- only look at relevant dates 
  drugdb <- drugdb[drugdb$eksd <= as.Date(dpp$period[2]) & drugdb$eksd >= as.Date(dpp$period[1]), ]
  
  ##--- only include relevant treatments, store them as list
  indata <- lapply(1:length(dpp$drugs), function(i) 
    dpp$drugdb[dpp$drugdb$atc %in% dpp$drugs[[i]]$atc, ])

  names(indata) <- names(dpp$drugs)
  
  ##--- any treatments not found in data? 
  trna <- sapply(indata, function(x) dim(x)[1] == 0)
  print(cat("------------------------------","\n"))
  print(cat("The following treaments were not found in input database:",
            paste(names(trna)[trna], collapse = ", "), "\n"))
  print(cat("------------------------------","\n"))

  return(indata)
} 



