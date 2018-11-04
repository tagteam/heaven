'add<-' <- function(object,subObject=NULL,var,value){
    .class <- class(object)
    stopifnot(.class %in% c("dpp", "dppObject"))
    if(.class=="dppObject"){
        if(length(subObject)>0) message(paste0("subOject=\"", subObject, "\" ignored."))
        object[[var]] <- value
    }
    else{
        if(length(subObject)==0) stop("No subObject specified.")
        object$subObject[[var]] <- value
    }
    return(object)
}


