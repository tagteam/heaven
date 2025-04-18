##' @title Add atc codes to object.
##'
##' @description Add atc codes to object.
##' @param value The dose sizes corresponding to those found in data. Note that the same value should never be repeated.  
##' @param min The corresponding minimal doses (same order as 'value').
##' @param max The corresponding maximal doses (same order as 'value'). 
##' @param def The corresponding default doses (same order as 'value'). 
##' @examples
##' 
##' d <- dpp()
##' drug(d, "treatment1") <- atc("A12B")
##' drug(d, "treatment1") <- pack(c(750, 75), 
##'                               min = c(250, 25),
##'                               max = c(1000, 100),
##'                               def = c(750, 100))
##' 
##' @author Helene Charlotte Rytgaard 
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

