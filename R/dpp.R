### dpp.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 14 2018 (13:53) 
## Version: 
## Last-Updated: Oct 22 2018 (07:42) 
##           By: Thomas Alexander Gerds
##     Update #: 31
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Create data preprocessing object 
##'
##' Create data preprocessing object (dpp)
##' @title Create data preprocessing object (dpp)
##' @author Helene Charlotte Rytgaard, Anders Munch & Thomas Alexander Gerds 
##' @export
dpp <- function(study=NULL,raw=NULL,id="pnr",baseline.date) {
    x = structure(list(study=list(),  ## study population: dt with pnr
                       regis=list(), ## instructions for raw data
                       variables=list(), ## instructions for variables
                       inclusion=list(), ## instructions for inclusion
                       exclusion=list(), ## instructions for exclusion
                       bsl=list(), ## instructions for baseline data/dates
                       fup=list(), ## followup data
                       info = list(id=id,bsl.dates=NULL), ## additional info 
                       tasks=list(), ## tasks
                       log=list() ## progress info
                       ), 
                  class = "dpp")
    return(x)
}

##' Register a register
##'
##' Register a register 
##' @param x dpp object
##' @param name name of register. defaults to \code{"Register"}.
##' @param id variable to identify subjects across the registers. any other name than \code{"pnr"}
##'        will internally be set to \code{"pnr"}. Note this will by side effect change the name
##'        of id variable if the input is a \code{data.table}.
##' @param variables select variables from register.
##' @param value either a data.table or path to a file.
##' @param ... additional arguments passed to \code{read.regis} for
##'     reading register from file.
##' @export
'addData<-' <- function(x,name="Register",id="pnr",variables=NULL,value,...){
    stopifnot(class(x)=="dpp")
    ## check in: one or several registers
    cl <- class(value)[1]
    if (cl=="data.frame") {
        value <- data.table(value)
        cl <- "data.table"
    }
    value <- switch(cl,"data.table"={
        if (!is.null(variables))
            regis <- list(value[,c(variables,id),with=FALSE])
        else{
            stopifnot(match(id,names(value),nomatch=0)>0)
            if (id!="pnr") setnames(value,id,pnr)
            regis <- list(value)
        }
        names(regis) <- name
    },"character"={
        if (file.exists(value)){
            regis <- list(list(file=value,variables=variables,...))
            names(regis) <- "name"
        }else{
            stop(paste0("File does not exist: ",value))
        }
    },{
        stop(paste0("Don't know how to read registry from class: ",cl))
    })
    x$regis <- c(regis,x$regis)
    names(x) <- make.unique(names(x))
    return(x)
}

read.register <- function(file,id,variables,...){
    ext <- tools::file_ext(value)
    regis <- switch(ext,"sas7bdat"={
        regis <- do.call("importSAS",list(filename=file,",keep=",unique(c(id,variables)),...))
    },"csv"={
        line1 <- do.call("fread",file=file,nrows=2)
        if (length(variables)==0){
            cc <- rep("character",NCOL(line1))
        }else{
            cc <- rep("NULL",NCOL(line1))
        }
        names(cc) <- names(line1)
        rm(line1)
        cc[grep(unique(c(id,variables)),names(cc))] <- "character"
        regis <- do.call("fread",file=file,nrows=Inf,colClasses=cc,...)
    })
    return(regis)
}

##' @export
'addBaseline<-' <- function(x,name,date){
    bslnames <- names(x$bsl)
    if (match(name,bslnames,nomatch=0)>0){
        stop(paste0("Already register a baseline dataset with name: ",name))
    }
    regis <- list(list(as.Date(date)))
    names(regis) <- name
    x$bsl <- c(regis,x$bsl)
}

## low-end all purpose function to
## add a variable, more precisely instructions (a function) 
## which creates the variable (can use one or all of the regis data)
##' @export
'addVariable<-' <- function(x,
                            name,
                            data, 
                            target,
                            value,
                            depends=NULL,
                            by=NULL,
                            priority=c("late","early")){
    priority=match.arg(priority)
    stopifnot(is.function(value) | class(value)[1]=="formula")
    if(missing(name)){
        stop("Sigh. Of course, a variable needs a name.")
    }
    var <- list(value,"target"=target,"depends"=depends,"by"=by)
    names(var)[1] <- name
    if (priority=="early"){
        x$variable <- c(var,x$variable)
    } else{
        x$variable <- c(x$variable,var)
    }
    return(x)
}


## an inclusion criterion selects pnr numbers from one or several
## of the registered data sets
##' @export
'addInclusion' <- function(x,name,target="all",value,priority=c("late","early")){
    priority=match.arg(priority)
    inc <- list(value)
    names(inc) <- name
    if(priority=="early"){
        x$inclusion <- c(inc,x$inclusion)
    } else{
        x$inclusion <- c(x$inclusion,inc)
    }
}
## an exclusion criterion removes pnr numbers from one or several
## of the processed data sets
##' @export
'addExclusion' <- function(x,name,target="all",value,priority=c("late","early")){
    priority=match.arg(priority)
    excl <- list(value)
    names(excl) <- name
    if(priority=="early"){
        x$exclusion <- c(excl,x$exclusion)
    } else{
        x$exclusion <- c(x$exclusion,excl)
    }
}

##' @export
firstAdmission <- function(data,var,expression,sortkey,by="pnr",...){
    d <- data[grepl(expression,data[[var]],...)]
    if (NROW(d)>0){
        setkeyv(d,c(by,sortkey))
        d[d[,.I[1],by=c(by,sortkey)]]
    }else{
        NULL
    }
}

##' @export 
do <- function(x,n=Inf,...){
    
}

######################################################################
### dpp.R ends here
