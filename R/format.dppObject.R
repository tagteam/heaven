format.dppObject <- function(dppObject,...,autoformat=TRUE){
    .status <- attr(dppObject, "status")
    if(.status=="empty" | .status=="nominal"){
        message("No data loaded, no formatting performed.")
    }
    if(.status=="loaded"){
        update.names <- list(...)
        format.list <- attr(dppObject,"formats")
        if(length(update.names)>0){
            if(!all(names(update.names) %in% names(format.list))){
                stop("Some names to update were not found.")
            }
            else{
                ## browser()
                names(format.list[names(update.names)]) <- as.vector(unlist(update.names))
            }
        }
        format.check <- internalFormat(dppObject$data,
                                        update.names=setNames(names(update.names), as.vector(unlist(update.names))),
                                        update.classes=format.list,
                                        silent=TRUE,
                                        autoformat=autoformat)
        out.mess <- format.check$message
        if(grepl("check.error",format.check$status)){
            out.mess <- paste0("Some of the variables do not have the correct name or format. See details below.\n",
                               out.mess)
        }
        if(out.mess!=""){
            message(out.mess)
        }
    }
}
