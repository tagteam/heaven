### baseline.dpp.R --- 
#----------------------------------------------------------------------
## Author: Anders Munch & Thomas Alexander Gerds
## Created: Sep 23 2018 (18:45) 
## Version: 
## Last-Updated: Oct 21 2018 (19:26) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:


## define structure
##  study.pop contain unique id's
##  raw.data are links to raw SAS data files or data.tables
##  
baseline.dpp <- function(dt=NULL,baseline.date,id="pnr",obs=Inf){
    if(!(id %in% names(dt)))
        stop(paste0("'", id, "' were not found in the data.table."))
    out <- structure(list(study.pop=dt,
                          raw.data=list(),
                          variable=list(),
                          id.vars=list(study.pop=id)),
                     class="baselineConstruct")
    if(!missing(baseline.date)){
        if(!(baseline.date %in% names(dt)))
            stop(paste0("'", baseline.date, "' were not found in the data.table."))
        attr(out, "baseline.date") <- baseline.date
    }
    unique.id <- length(unique(dt[[id]]))==nrow(dt)
    if(!unique.id)
        warning("The id's are not unique. This will probably become a problem.")
    return(out)
}


## 'addBaselineDate<-' <- function(x,value){
    ## stopifnot(class(x)=="baselineConstruct",
              ## value%in%names(bc$study.pop))
    ## attr(x,"baseline.date") <- value
    ## return(x)
## }



## high-end functions
wrap.fun <- function(fun.name,update.args,fixed.args){
    tmp.input <- paste(paste(names(update.args),update.args,sep="="),collapse=",")
    if(!missing(fixed.args)){
        ind.char <- sapply(fixed.args, is.character)
        char.args <- fixed.args[ind.char]
        fixed.args[ind.char] <-  lapply(char.args, function(x) paste0("\"",x,"\""))
        tmp.input <- paste0(tmp.input,",",
                            paste(paste(names(fixed.args),fixed.args,sep="="),collapse=","))
    }
    tmp.text <- paste0("function(",paste(update.args,collapse=", "),") ",
                       fun.name,
                       "(",tmp.input,")")
    return(eval(parse(text=tmp.text)))
}





'addInterval<-' <- function(x,cond.name,var,output="logical",value){
    stopifnot(class(x)=="baselineConstruct")
    if(is.null(attr(x,"baseline.date")))
        stop("Specify a variable to use as baseline date.")
    if(missing(cond.name))
        cond.name <- next.name(names(x$variable),"Variable")
    baseline.date <- attr(x,"baseline.date")
    ## Copy-paste the function -- any better solution?
    ## tmp.fun <- protoInterval(baseline.date,var,interval=value,output=output)
    protoInterval <- function(baseline.date,var,interval,output){
        stopifnot(output %in% c("logical","last.date","days.since.last"))
        baseline.date <- unique(baseline.date)
        if(length(baseline.date)>1) stop("The baseline.date(s) are not unique")
        base.cond <- (baseline.date-interval<var & var<baseline.date)
        if(output=="logical")
            return(any(base.cond))
        else{
            if(any(base.cond))
                last.date <- max(var[base.cond],na.rm=TRUE)
            else
                last.date <- as.Date(NA)
        }
        if(output=="last.date")
            return(last.date)
        if(output=="days.since.last")
            return(baseline.date-last.date)
    }
    tmp.f <- wrap.fun("protoInterval",
                      update.args=list(baseline.date=baseline.date,var=var),
                      fixed.args=list(interval=value,output=output))
    variable(x,cond.name) <- tmp.f
    return(x)
}

'addAge<-' <- function(x,value){
    stopifnot(class(x)=="baselineConstruct")
    if(is.null(attr(x,"baseline.date")))
        stop("Specify a variable to use as baseline date.")
    baseline.date <- attr(x,"baseline.date")
    protoAge <- function(baseline.date,birth.date){
        baseline.date <- unique(baseline.date)
        birth.date <- unique(birth.date)
        stopifnot(length(baseline.date)==1)
        if(!(length(birth.date)==1))
            stop("The birth.dates are not unique defined for each individual.")
        return(as.numeric(baseline.date-birth.date)/365.25)
    }
    tmp.f <- wrap.fun("protoAge",
                      update.args=list(baseline.date=baseline.date,birth.date=value))
    variable(x,cond.name="Age") <- tmp.f
    return(x)
}


'addDeath<-' <- function(x,scale="days",value){
    stopifnot(class(x)=="baselineConstruct")
    if(is.null(attr(x,"baseline.date")))
        stop("Specify a variable to use as baseline date.")
    baseline.date <- attr(x,"baseline.date")
    protoDeath <- function(baseline.date,death.date,scale="days"){
        baseline.date <- unique(baseline.date)
        death.date <- unique(death.date)
        stopifnot(length(baseline.date)==1,
                  scale %in% c("days","months","years"))
        if(!(length(death.date)==1))
            stop("The death.dates are not unique defined for each individual.")
        time.to.death <- as.numeric(death.date-baseline.date)
        if(scale=="days") return(time.to.death)
        if(scale=="months") return(time.to.death/30.45)
        if(scale=="years") return(time.to.death/365.25)
    }
    tmp.f <- wrap.fun("protoDeath",
                      update.args=list(baseline.date=baseline.date,death.date=value),
                      fixed.args=list(scale=scale))
    cond.name <- paste0("Time to death (", scale, ")")
    variable(x,cond.name) <- tmp.f
    return(x)
}



## set restrictions either local for a single variable or
## global for one of the raw datasets
'addRestr<-' <- function(bc,object,type=NULL,value){
    stopifnot(is.null(type)| type %in% c("data","var"),
              class(bc)=="baselineConstruct")
    data.names <- names(bc$raw.data)
    var.names <- names(bc$variable)
    if(!object %in% c(data.names,var.names))
        stop(paste(object, "not found in", bc))
    if((object %in% data.names) & (object %in% var.names)){
        if(is.null(type))
            stop(paste0("Both a data and variable object found in '", bc,
                        "'. Supply 'type' equal to 'data' or 'var' so select which object to use."))
        else{
            if(type=="data")
                attr(bc$raw.data[[object]], "restriction") <- value
            else
                attr(bc$variable[[object]], "restriction") <- value
        }
    }
    else{
        if(object %in% data.names)
            attr(bc$raw.data[[object]], "restriction") <- value
        else
            attr(bc$variable[[object]], "restriction") <- value
    }
    return(bc)
}

## run preprocessing
constructData <- function(x,obs=Inf){
    stopifnot(class(x)=="baselineConstruct")
    datasets <- lapply(x$raw.data, function(x) head(x, obs))
    base <- head(copy(x$study.pop),obs) #NB
    variable <- x$variable
    id.vars <- x$id.vars
    base.id <- id.vars[["study.pop"]]
    ## Paste the comorb datasets onto the base
    base.merge <- base
    for(dt in names(datasets)){
        ## Check if there is a restriction
        if(is.null(attr(datasets[[dt]],"restriction")))
            restr.statement <- parse(text="TRUE")
        else
            restr.statement <- parse(text=attr(datasets[[dt]],"restriction"))
        tmp.merge <- merge(base,datasets[[dt]],
                           by.x=base.id, by.y=id.vars[[dt]], all.x=TRUE)
        base.merge <- rbind(base.merge,tmp.merge[eval(restr.statement)],fill=TRUE)
    }
    ## Check and run conditionless functions
    condless <- lapply(variable, function(z) is.null(attr(z, "restriction")))
    condwith <- variable[!unlist(condless)]
    condless <- variable[unlist(condless)]
    ## Maybe this collect.function is not the most efficient way to do it? ... test
    collect.function <- function(list.fun,...){
        input.args <- list(...)
        out.list <- list()
        for(j in 1:length(list.fun)){
            part.args <- formalArgs(list.fun[[j]])
            part.args <- part.args[part.args %in% names(input.args)]
            out.list <- append(out.list,
                               list(do.call(list.fun[[j]],input.args[part.args])))
        }
        names(out.list) <- names(list.fun)
        return(out.list)
    }
    if(length(condless)>0){
        tmp.fun <- function(...) collect.function(condless,...)
        total.args <- unlist(sapply(condless,formalArgs))
        names(total.args) <- NULL
        total.args <- unique(total.args)
        out.base <- base.merge[,do.call(tmp.fun,.SD),by=base.id,.SDcols=total.args]
    }
    else
        out.base <- base
    ## Run the functions that have condtions on restricted datasets
    if(length(condwith)>0){
        for(cc in names(condwith)){
            restr.statement <- parse(text=attr(condwith[[cc]],"restriction"))
            ## browser()
            tmp.dt <- base.merge[eval(restr.statement),do.call(condwith[[cc]],.SD),
                                 by=base.id,.SDcols=formalArgs(condwith[[cc]])]
            names(tmp.dt)[2] <- cc
            out.base <- merge(out.base,tmp.dt,by=base.id,all.x=TRUE)
        }
        out <- out.base
    }
    else
        out <- out.base
    return(out[])
}

## print results
print.baselineConstruct <- function(bc){
    baseline.date <- attr(bc,"baseline.date")
    id.vars <- bc$id.vars
    # Print basis information
    cat(paste0("The baseline dataset contains the variables:\n  ",
               paste(names(bc$study.pop),collapse=", "),"\n",
               "The data will be merged on the variable '", id.vars[["study.pop"]], "'.\n"))
    if(is.null(baseline.date))
        cat("No baseline date is specified.\n")
    else
        cat(paste0("The variable to use as baseline date is '", baseline.date, "'.\n"))
    if(!length(bc$raw.data))
        cat("No other data are stored.\n")
    else{
        cat("==========================================\n\n")
        cat("In addition the following data are stored:\n")
        for(i in names(bc$raw.data)){
            cat(paste0("- ", i,", containing the variable(s):\n    ",
                       paste(names(bc$raw.data[[i]]),collapse=", "),"\n",
                       "  The data will be merged on the variable '", id.vars[[i]], "'.\n"))
            if(!is.null(attr(bc$raw.data[[i]],"restriction")))
                cat(paste0("  Only the subset fullfilling\n",
                           "         ", attr(bc$raw.data[[i]],"restriction"),
                           "\n  will be used.\n"))
        }
    }
    if(length(bc$variable)==0)
        cat("No variables are defined")
    else{
        cat("==========================================\n\n")
        cat("The following variable(s) are constructed:\n")
        cat("------------------------------------------\n\n")
        for(i in names(bc$variable)){
            ## if(is.null(attr(bc$variable[[i]],"restriction")))
            ##    cat(paste0("'", i, "' is constructed as:\n"))
            ## else{
            ##     cat(paste0("'", i, "' is constructed form the subset fulfilling:\n"))
            ##     cat("-------\n")
            ##     cat("         ")
            ## cat(paste(attr(bc$variable[[i]],"restriction"),"\n"))
            ## cat("-------\n")
            ##     cat("as:\n")
            ## }
            cat(paste0("'", i, "' is constructed as follows:\n"))
            ## cat("-------\n")
            if(!is.null(attr(bc$variable[[i]],"restriction"))){
                cat("Use subset fullfilling\n")
                cat("         ")
                cat(paste0(attr(bc$variable[[i]],"restriction"),"\n"))
                cat("to construct the variable as:\n")
            }
            printFunConstruct <- function(fun){
                cat("-------\n")
                cat(paste0("Input :  (", paste(formalArgs(fun),collapse=", "), ")"))
                cat("\nOutput:  ")
                print(body(fun))
                cat("-------\n")
            }
            printFunConstruct(bc$variable[[i]])
            cat("\n")
        }
    }
}

### helper functions

char.is.num <- function(x){
    sapply(strsplit(sub("\\.","",x),""),
           function(a) all(a %in% 0:9))
}




######################################################################
### baseline.dpp.R ends here
