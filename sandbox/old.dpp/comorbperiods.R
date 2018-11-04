'comorbperiods' <- function(value,
                            id='pnr',
                            atc='atc',
                            startdate='startdate',
                            enddate='enddate',
                            obs=Inf,
                            autoformat=TRUE,
                            ...){
    if(!is.character(value) & !is.data.table(value)){
        stop("Supplied value should be either a string or a data.table.")
    }
    ## Below should be moved to the general construction of dpp
    comorbperiods <- structure(list(data=NULL,nominal=NULL),
                               class="dppObject",
                               status="empty",
                               names=NULL,
                               nobs=Inf,
                               formats=list(id="numeric",atc="character",startdate="Date",enddate="Date"))

    ## (Need also to make this a special kind of dppObject, to include non-dt.)
    attr(comorbperiods, "nobs") <- obs
    args <- list(...)
    update.names <- list(id=id,atc=atc,startdate=startdate,enddate=enddate)
    if(is.character(value)){
        value <- paste0(getwd(),"/",value)
        comorbperiods$nominal <- list(path=value,
                                      names=update.names,
                                      import.info = args)
        comorbperiods$data <- NULL
        attr(comorbperiods, "status") <- "nominal"
    }else{
        comorbperiods$data <- value
        comorbperiods$nominal <- NULL
        attr(comorbperiods, "status") <- "loaded"         
        format.list <- attr(comorbperiods,"formats")
        names(format.list) <- as.vector(unlist(update.names))
        do.call(format, c(list(dppObject=comorbperiods,autoformat=autoformat),
                          update.names))
    }
    return(comorbperiods)
}
