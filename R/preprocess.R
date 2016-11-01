##' Update database
##' 
##' Update database
##' @title Update database
##' @param dpp a
##' @param id a
##' @param trace a
##' @author Helene Charlotte Rytgaard
##' @export
`preprocess` <- function(dpp, id = NULL, trace = FALSE) {

  ##--- only look at id=, if specified
  if (length(id) > 0) {
    drugdb <- dpp$drugdb[dpp$drugdb$id %in% id, ]
    admdb  <- dpp$admdb[dpp$admdb$id %in% id, ]
  } else {
    drugdb <- dpp$drugdb
    admdb  <- dpp$admdb
  }
  
  ##--- only look at relevant dates 
  drugdb <- drugdb[drugdb$pdate <= as.Date(dpp$period[2]) & drugdb$pdate >= as.Date(dpp$period[1]), ]
  
  ##--- only include relevant treatments, store them as list
  indata <- lapply(1:length(dpp$drugs), function(i) 
    drugdb[drugdb$atc %in% dpp$drugs[[i]]$atc, ])

  names(indata) <- names(dpp$drugs)
  
  ##--- any treatments not found in data? 
  if (trace) {
    trna <- sapply(indata, function(x) dim(x)[1] == 0)
    if (sum(trna) > 0) {
      print(cat("------------------------------","\n"))
      print(cat("The following treatments were not found in input database:",
                paste(names(trna)[trna], collapse = ", "), "\n"))
      print(cat("------------------------------","\n"))
    }
  }
  
  return(indata)
} 



