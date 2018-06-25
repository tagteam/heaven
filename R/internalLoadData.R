##' Internal function to load data into dpp.
##'
##' Internal function used to load data into dpp.
##' 
##' The function checks if the dppObject is already loaded or only specified nominally (i.e., as a path to the data). If the latter is the case, the data is loaded; in any case the data is formatted, or at least tried to be formatted correctly according to the information in the dppObject.
##' The output is then the formatted dppObject and information about possible format or name conflicts.
##' @title internalLoadDate
##' @param dppObject dppObject in the status "loaded" or "nominal". 
##' @return The output is a list consisting of: (1) The loaded and formatted \code{dppObject}, (2) a vector of strings denoting keywords about which kind of formatting problems (if any) occurred, and (3) the whole detailed messages about which variable(s) were problematic (if any).
##' ##' @author Anders Munch
internalLoadData <- function(dppObject){
    stopifnot(attr(dppObject,"status")%in%c("loaded","nominal"))
    update.names <- NULL
    if(attr(dppObject,"status")!="loaded"){
        .nom <- dppObject$nominal
        update.names <- .nom$names
        if(grepl(".sas",.nom$path)){
            dppObject$data <- do.call(importSAS,c(list(filename=.nom$path), .nom$import.info))
        }else{
            dppObject$data <- do.call(fread,c(list(input=.nom$path), .nom$import.info))
        }
        attr(dppObject,"status") <- "loaded" 
    }
    ## Do the formatting
    out.mess <- ""
    format.list <- attr(dppObject,"formats")
    if(length(update.names)>0){
        if(!all(names(update.names) %in% names(format.list))){
            out.mess <- paste0(out.mess, "Some names to update were not found.")
        }
        else{
            names(format.list[names(update.names)]) <- as.vector(unlist(update.names))
        }
    }
    if(out.mess==""){ # Else, something is too wrong to continue
        format.check <- internalFormat(dppObject$data,
                                        update.names=setNames(names(update.names), as.vector(unlist(update.names))),
                                        update.classes=format.list,
                                        silent=TRUE)
        dppObject$data <- format.check$dt
        out.mess <- format.check$message
        if(grepl("check.error",format.check$status)){
            out.mess <- paste0("Some of the variables do not have the correct name or format. See details below.\n",
                               out.mess)
        }
    }
    return(list(dppObject=dppObject,prob.status=grepl("check.error",format.check$status),message=out.mess))
}
