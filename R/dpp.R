##' Create data preprocessing object 
##'
##' Create data preprocessing object (dpp)
##' @title Create data preprocessing object (dpp)
##' @author Helene Charlotte Rytgaard
##' @export
dpp <- function() {
    out = structure(list(drugdb   = NULL,
                         admdb    = NULL, 
                         drugs    = NULL#, 
                         #period   = c(1, 1e10),
                         #N        = 2, 
                         #maxdepot = 10
                         ),
                    class = "dpp")
    out
}

