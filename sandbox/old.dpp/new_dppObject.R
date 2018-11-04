##' Internal function to setup new dppObject.
##'
##' Internal function to create dppObject with some structure. It is only used through other functions.
##' @title new_dppObject
##' @param formats Specify the formats of the dppObject. Should be a named list of formats.
##' @return An object of class \code{dppObject} with specified formats but otherwise empty. 
##' @author Anders Munch
new_dppObject <- function(formats){
    structure(list(data=NULL,
                   nominal=NULL),
              class="dppObject",
              status="empty",
              names=NULL,
              nobs=Inf,
              formats=formats)
}
