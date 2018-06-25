##' Internal function for formatting dppObjects.
##'
##' Internal function used in, e.g., format.dppObject.
##' It takes a data.table as input a tries to format it and/or checks the format.
##'
##' The output is the (formated) \code{data.table} and a to string of information: One stating which kind of formatting problems (if any) was encountered, and a detailed description of the problems. 
##'
##' Note: The names are first updated according to \code{update.names} and then the classes are updated according to \code{update.classes}; thus \code{update.classes} should contain the updated names. 
##' @title internalFormat
##' @param dt \code{data.table} to format or check format for. 
##' @param update.names Named list of names to updates. 
##' @param update.classes Named list of variables to update (change the \code{class} for these variables). 
##' @param autoformat If \code{TRUE} the function will try to format \code{dt}. Otherwise it will just check the formats to see if they match the ones specified in in \code{update.classes}. The names are always updated if \code{update.names} is non-empty. 
##' @param silent If \code{TRUE} the function will print a message if there was any changes or problems with the formatting. 
##' @return The output is a list consisting of: (1) The (formatted) \code{data.table}, (2) a vector of strings denoting keywords about which kind of formatting problems (if any) occurred, and (3) the whole detailed messages about which variable(s) were problematic (if any). 
##' @author Anders Munch
internalFormat <- function(dt,update.names=NULL,update.classes=NULL,autoformat=TRUE,silent=FALSE){
    out.mess <- .status <- ""
    on.exit({
        if(out.mess!="" & !silent) message(out.mess)
    })
    ## Pick out only those names are safe to update (does not exist beforehand). 
    update.names <- update.names[!(as.vector(unlist(update.names)) %in% names(dt))]
    if(length(update.names)>0){
        .nind <- names(update.names) %in% names(dt)
        setnames(x=dt, old=names(update.names[.nind]),
                 new=as.vector(unlist(update.names))[.nind])
        if(any(!.nind)){
            .status <- paste(.status, "update.name.error")
            out.mess <- paste0(out.mess,
                               "\nThe variable(s) '",
                               paste(names(update.names)[!.nind],collapse="', '"),
                               "' were not found in dataset and could thus not be updated.\n")
        }
    }
    if(length(update.classes)){
        if(autoformat){ ## First try to automatically update classes
            fw.good <- fw.bad <- ""
            for (.n in names(update.classes)){
                if(.n %in% names(dt)){
                    tryval <- try({
                        .oldclass <- class(dt[[.n]])
                        if(.oldclass!=update.classes[.n]){
                            .format.expr <- parse(text=paste0("as.", update.classes[.n],
                                                              "(dt[['", .n, "']])"))
                            set(dt, j=.n, value=eval(.format.expr))
                        }
                    },TRUE)
                    if(inherits(tryval, "try-error")){
                        fw.bad <- paste0(fw.bad,
                                         "  - ", .n, " could not be converted to '",
                                         update.classes[[.n]], "'\n")
                    }else{
                        if(.oldclass!=update.classes[.n]){
                            fw.good <- paste0(fw.good,
                                              "  - ", .n, " converted from '",
                                              .oldclass, "' to '",
                                              update.classes[[.n]], "'\n")
                        }
                    }
                }
            }
            if(fw.good!="") {
                out.mess <- paste0(out.mess,
                                   "\nThe following classes where changed succesfully:\n",
                                   fw.good, "\n")
            }
            if(fw.bad!="") {
                .status <- paste(.status, "autoformat.error")
                out.mess <- paste0(out.mess,
                                   "The following classes could *not* be changed:\n",
                                   fw.bad, "\n")
            }
        }
        check.warning <- ""
        for(.n in names(update.classes)){
            if(!(.n %in% names(dt))){
                check.warning <- paste0(check.warning,
                                        "  - ", .n, " was not found\n")
            }else{
                if(update.classes[[.n]]!=class(dt[[.n]])){
                    check.warning <- paste0(check.warning,
                                            "  - ",
                                            .n, " is of class ", class(dt[[.n]]),
                                            ", but should be ", update.classes[[.n]],
                                            ".\n")
                }
            }
        }
        if (check.warning!="") {
            .status <- paste(.status, "check.error")
            check.warning <- paste0("Detected the following conflicts:\n", check.warning)
        }
        out.mess <- paste0(out.mess, "\n", check.warning)
    }
    if(out.mess=="\n"){out.mess <- ""}
    return(list(dt=dt,status=.status,message=out.mess))
}
