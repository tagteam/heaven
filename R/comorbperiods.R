'comorbperiods<-' <- function(dpp,
                              id='pnr',
                              atc='atc',
                              startdate='startdate',
                              enddate='enddate',
                              obs=Inf,
                              autoformat=TRUE,
                              ...,
                              value){
    if(!is.character(value) & !is.data.table(value)){
        stop("Supplied value should be either a string or a data.table.")
    }
    ## Below should be moved to the general construction of dpp
    dpp$comorbperiods <- new_dppObject(formats=list(id="numeric",
                                                    atc="character",
                                                    startdate="Date",
                                                    enddate="Date"))
    ## (Need also to make this a special kind of dppObject, to include non-dt.)
    attr(dpp$comorbperiods, "nobs") <- obs
    args <- list(...)
    update.names <- list(id=id,atc=atc,startdate=startdate,enddate=enddate)
    if(is.character(value)){
        dpp$comorbperiods$nominal <- list(path=value,
                                          names=update.names,
                                          import.info = args)
        dpp$comorbperiods$data <- NULL
        attr(dpp$comorbperiods, "status") <- "nominal"
    }else{
        dpp$comorbperiods$data <- value
        dpp$comorbperiods$nominal <- NULL
        attr(dpp$comorbperiods, "status") <- "loaded"         
        format.list <- attr(dpp$comorbperiods,"formats")
        names(format.list) <- as.vector(unlist(update.names))
        do.call(format, c(list(dppObject=dpp$comorbperiods,autoformat=autoformat),
                          update.names))
    }
    return(dpp)
}
