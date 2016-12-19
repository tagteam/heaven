##' Update preprocessing object with new drug.
##'
##' @title Add new drug
##' @param dpp Data preprocessing object
##' @param name Name of the drug. (Just write, don't use "") 
##' @param add Logical variable. Per default set to FALSE. If TRUE, and drug with name already exists, then this is not overwritten.  
##' @usage drug(d, drugname1) <- value
##' @return The data preprocessing object is updated. 
##' @author Helene Charlotte Rytgaard
##' @export
`drug<-` <- function(dpp, drugname, add = FALSE, value) {
  
  i <- (1:length(dpp$drugs))[names(dpp$drugs) == drugname]

  dpp$drugs = value(dpp, add, i, drugname)  
  
  return(dpp)
}  



##' @export
atc <- function(atc) {
  
  f <- function(dpp, add, i, drugname) {
    if (length(i) > 0) {
      if (add)
        dpp$drugs[[i]]$atc = unique(c(dpp$drugs[[i]]$atc, atc))
      else
        dpp$drugs[[i]]$atc = atc
    }
    else { 
      dpp$drugs$drugname$atc = atc
      names(dpp$drugs)[length(dpp$drugs)] = drugname
    }
    return(dpp$drugs)
  }

  return(f)
}  


##' @export
pack <- function(value, min, max, def) {

  f <- function(dpp, add, i, drugname) {
    if (length(i) > 0) {
      if (add)
        dpp$drugs[[i]]$doses = list(value = c(dpp$drugs[[i]]$doses$value, value),
                                    min   = c(dpp$drugs[[i]]$doses$min, min),
                                    max   = c(dpp$drugs[[i]]$doses$max, max),
                                    def   = c(dpp$drugs[[i]]$doses$def, def))
      else
        dpp$drugs[[i]]$doses = list(value = value, min = min, max = max, def = def)
    } else { 
      dpp$drugs$drugname$doses = list(value = value, min = min, max = max, def = def)
      names(dpp$drugs)[length(dpp$drugs)] = drugname
    }
    
    if (length(dpp$drugs[[i]]$doses$value) > length(unique(dpp$drugs[[i]]$doses$value)))
      cat("WARNING: The same value has been specified more than once - inconsistencies may occur.")
    
    return(dpp$drugs)
  }
  
  return(f)
}  



