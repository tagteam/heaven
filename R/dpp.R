### dpp.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 14 2018 (13:53) 
## Version: 
## Last-Updated: Nov  3 2018 (18:04) 
##           By: Thomas Alexander Gerds
##     Update #: 90
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
##' @param ... additional arguments passed to \code{addData} for
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
    var <- list(list(instructions=value,
                "target"=target,
                "depends"=depends,
                priority=priority,
                "by"=by))
    names(var) <- name
    x$variable <- c(x$variable,var)
    return(x)
}


## an inclusion criterion selects pnr numbers from one or several
## of the registered data sets
##' @export
'addInclusion<-'  <- function(x,
                          name,
                          target="all",
                          value,
                          priority=c("late","early")){
    priority=match.arg(priority)
    inc <- list(value)
    names(inc) <- name
    if(priority=="early"){
        x$inclusion <- c(inc,x$inclusion)
    } else{
        x$inclusion <- c(x$inclusion,inc)
    }
    return(x)
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
selector <- function(data,
                     var,
                     by="pnr",
                     search.term,
                     sortkey, # "uddto",
                     period=NULL, # =list(variable=sortkey,start=as.Date("1970-01-01"),stop=NULL),
                     backward=NULL, #  = list(data, reference, variable, length)
                     forward=NULL,
                     select="first",
                     ...){
    ## this function will be evaluated in an environment that contains
    ## lpr
    if (missing(data)){
        stop(paste0(sample(c("Sorry","Honey","Sweety","Attagirl","Attaboy","My dear"),size=1),
                    ", we cannot search thin air. Need data to work with."))
    }else{
        if (!is.character(data)){
            stop(paste0(sample(c("I told you that",
                                 "Looking at the help page shows that",
                                 "Reading the docs would reveal that",
                                 "You forgot that",
                                 "Nice try, but"),size=1),
                        " data has\nto be the name of a data set, i.e., a character."))
        }
        data= as.character(substitute(data))
    }
    ## TODO: check arguments (period should be dates etc)
    f <- function(data,
                  var,
                  by,
                  search.term,
                  sortkey,
                  period,
                  backward,
                  forward,
                  select,
                  ...){
        try.d <- try(d <- eval(as.name(data)))
        if (class(try.d)[1]=="try-error"){
            stop(paste0(sample(c("Very sorry, but",
                                 "This is messed up somehow,",
                                 "Damn it,",
                                 "May I kindly remind you that",
                                 "Impossible to proceed, because"),size=1),
                        " data ",data," has not been checked in yet."))
        }
        if (NROW(d)>0){
            ## apply period
            if (!missing(period) && !is.null(period)){
                if (is.null(period$stop)){
                    if (is.null(period$start)){
                        # nothing happens
                    }else{ # start only
                        d <- d[(d[[period$variable]]>=period$start)]
                    }
                }else{
                    if (is.null(period$start)){ # stop only
                        d <- d[(d[[period$variable]]<=period$stop)]
                    }else{ # both
                        d <- d[(d[[period$variable]]>=period$start)&(d[[period$variable]]<=period$stop)]
                    }
                }
            }
            ## apply backward
            if (!missing(backward) && !is.null(backward)){
                if (is.null(backward$data)) {
                    bd="study"
                } 
                bd =eval(as.name(backward$data))
                if (!(by %in%names(bd))){
                    stop(paste0(sample(c("Oooh",
                                         "Uups",
                                         "Beginner mistake",
                                         "No no no no no",
                                         "Try again"),size=1),
                                ", the variable ",by," is not in reference data for backward search."))
                }
                # TODO: should check if bd has no duplicated by values
                setkeyv(bd,by)
                setkeyv(d,by)
                ## merge
                if (backward$reference%in% names(d)){
                    setnames(bd,backward$reference,"canttouchthis")
                    backward$reference <- "canttouchthis"
                }
                d <- bd[d,.(by,backward$reference)]
                ## select younger than this
                d <- d[d[[backward$reference]]-d[[period$variable]]>length]
            }
            if (NROW(d)>0){
                ## now sort  
                setkeyv(d,c(by,sortkey))
                ## filter
                if (!is.null(search.term)){
                    d <- d[grepl(search.term,d[[var]],...),.SD,.SDcols=c(by,var,sortkey)]
                }
                ## now select
                if (NROW(d)>0){
                    d <- switch(select,
                                "first"={
                                    d[d[,.I[1],by=c(by,sortkey)]$V1]},
                                "last"= {
                                    d[d[,.I[.N],by=c(by,sortkey)]$V1]
                                }, {
                                    d
                                })
                }
            }
        }
        return(d)
    }
    ## maybe switch to match.call at some pont
    ## attr(f,"arguments") <- match.call(expand.dots=TRUE)
    attr(f,"arguments") <- c(list(data=data,
                                  var=var,
                                  by=by,
                                  search.term=search.term,
                                  sortkey=sortkey,
                                  period=period,
                                  backward=backward,
                                  forward=forward,
                                  select=select), 
                             ...)
    return(f)
}
    

##' @export 
do <- function(x,n=Inf,verbose=TRUE,...){
    ## inclusion
    for (Inc in x$inclusion){
        x$study <- with(x$regis,do.call(Inc,attr(Inc,"arguments")))
        if (is.null(x$study)){
            message(paste0("\nWell, for some reason your search did not match any subject.\nPlease investigate the particularities of your inclusion criteri",ifelse(length(x$inclusion)>1,"a.","on."),"\n"))
        }else{
            message(paste0("\n",sample(c("Nice","Coolio","Well done","Wow","Not bad"),size=1),
                           ", your search matched ",NROW(x$study)," subjects."))
        }
    }
    ## exclusion
    ## baseline
    for (V in x$variable){
        x[[V$target]] <- with(x$regis,do.call(V$instructions,attr(V$instructions,"arguments")))    
    }
    browser()
    ## followup
    x
}

######################################################################
### dpp.R ends here
