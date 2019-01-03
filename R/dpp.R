### dpp.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 17 2018 (13:53) 
## Version: 
## Last-Updated: Nov 27 2018 (18:47) 
##           By: Thomas Alexander Gerds
##     Update #: 206
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
##' @author Helene Charlotte Rytgaard, Anders Munch & Thomas Alexander
##'     Gerds
##' @export
##' @param id Character. Name of subject id variable. Default is \code{"pnr"}
dpp <- function(id="pnr") {
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
##' @param ... additional arguments passed to \code{addData} for
##'     reading register from file.
##' @param value either a data.table or path to a file.
##' @export
'addData<-' <- function(x,
                        name="Register",
                        id="pnr",
                        variables=NULL,
                        ...,
                        value){
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
            if (id!="pnr") setnames(value,id,"pnr")
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
##' Read a register
##'
##' Read a register for data preprocessing purposes.
##' @title Read a registered register from SAS data file or csv file  
##' @param file file name
##' @param id Character. Subject id variable. Default is \code{"pnr"}.
##' @param variables Read only these variables.
##' @param ... Arguments passed to \code{importSAS} or \code{fread}.
##' @return data.table 
##' @seealso dpp 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
read.register <- function(file,id,variables,...){
    ext <- tools::file_ext(file)
    regis <- switch(ext,"sas7bdat"={
        regis <- do.call("importSAS",list(filename=file,",keep=",unique(c(id,variables)),...))
    },"csv"={
        line1 <- do.call(data.table::fread,list(file=file,nrows=2,...))
        if (length(variables)==0){
            cc <- rep("character",NCOL(line1))
        }else{
            cc <- rep("NULL",NCOL(line1))
        }
        names(cc) <- names(line1)
        rm(line1)
        cc[grep(unique(c(id,variables)),names(cc))] <- "character"
        regis <- do.call(data.table::fread,list(file=file,nrows=Inf,colClasses=cc,...))
    })
    return(regis)
}

##' Add a baseline to a dpp object
##'
##' Add a baseline to a dpp object
##' @title Add baseline
##' @param x dpp object
##' @param name name of baseline date
##' @param ... not (yet) used
##' @param value Instructions
##' @export
'addBaseline<-' <- function(x,name,...,value){
    bslnames <- names(x$bsl)
    if (match(name,bslnames,nomatch=0)>0){
        stop(paste0("Already register a baseline dataset with name: ",name))
    }
    names(value) <- name
    x$bsl <- c(value,x$bsl)
    x
}


##' Add a variable to a dpp object
##'
##' All purpose function to add a variable, more precisely
##' instructions (a function) which creates the variable (can use one
##' or all of the regis data)
##' @param x dpp object 
##' @param name name of variable
##' @param data Optional the name of the data table in which to evaluate \code{value}.
##' @param target Character. The name of a baseline date or \code{"fup"} for the followup dataset. Where to store the result.
##' @param depends vector of names of other instructions (inclusion, exclusion, variables) that should be evaluated before this one.
##' @param priority Processing priority. Maybe changed by \code{depends}.
##' @param by Character vector. Specifying subsets in which \code{value} is evaluated. E.g., if set to \code{"pnr"} then \code{value} is applied to the data of each subject separately. 
##' @param ... not (yet) used
##' @param value A function obtained with the all-purpose function \code{selector} or one of the specialized functions. See \code{dpp}.
##' @export
'addVariable<-' <- function(x,
                            name,
                            data, 
                            target,
                            depends=NULL,
                            priority=c("late","early"),
                            by=NULL,
                            ...,value){
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


##' Instructions for a \code{dpp} object regarding which subjects to include
##'
##' An inclusion criterion selects pnr numbers from one or several of the registered data sets
##' instructions (a function) which creates the variable (can use one
##' or all of the regis data)
##' @title Inclusion instructions
##' @param x dpp object
##' @param name name of inclusion criterion
##' @param target currently ignored
##' @param priority Character. Data preprocessing time point. Either \code{"late"} or \code{"early"}. 
##' @param ... not (yet) used
##' @param value A function obtained with the all-purpose function \code{selector} or one of the specialized functions. See \code{dpp}.
##' @export
'addInclusion<-'  <- function(x,
                              name,
                              target="all",
                              priority=c("late","early"),...,value){
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

##' Instructions for a \code{dpp} object regarding which subjects to include
##'
##' An exclusion criterion removes pnr numbers from one or several of the processed data sets.
##' @title Inclusion instructions
##' @param x dpp object
##' @param name name of inclusion criterion
##' @param target currently ignored
##' @param priority Character. Data preprocessing time point. Either \code{"late"} or \code{"early"}. 
##' @param ... not (yet) used
##' @param value A function obtained with the all-purpose function \code{selector} or one of the specialized functions. See \code{dpp}.
'addExclusion<-'  <- function(x,
                           name,
                           target="all",
                           priority=c("late","early"),
                           ...,
                           value){
    priority=match.arg(priority)
    excl <- list(value)
    names(excl) <- name
    if(priority=="early"){
        x$exclusion <- c(excl,x$exclusion)
    } else{
        x$exclusion <- c(x$exclusion,excl)
    }
    return(x)
}

##' All purpose function to contstruct instructions for inclusion, exclusion and variables
##'
##' Returns instructions (a function) which creates variables from one or all of the regis data sets.
##' @title Select variables 
##' @param data Optional. The name of a registered data set in which to evaluate the instructions.
##' @param by Character vector. Specifying subsets in which \code{value} is evaluated. E.g., if set to \code{"pnr"} then \code{value} is applied to the data of each subject separately. 
##' @param var Variable to filter with. Could be \code{"diag"} when data is set to \code{"lpr"}.
##' @param search.term Expression to filter with. Could be (partial) ATC or (partial) ICD codes pasted together with \code{|}.
##' @param sortkey Date variable for sorting in time. Could be \code{"uddto"} when data is \code{"lpr"}
##' @param period Calendar time period 
##' @param backward Object from which to look backwards in time.
##' @param forward Object from which to look forwards in time. 
##' @param select Either a character ("first", "last", "any", "count") or a function.
##' @param name Name or vector of names for the new variable(s).
##' @param collect Named vector or list with variables to collect 
##' @param ... Arguments passed to the result 
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
                     name=NULL,
                     collect=NULL,
                     ...){
    .SD=.I=.N=NULL
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
                  name,
                  collect,
                  ...,
                  environment){
        try.d <- try(d <- eval(as.name(data),envir=environment))
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
                d <- bd[d,data.table::data.table(by,backward$reference)]
                ## select younger than this
                d <- d[d[[backward$reference]]-d[[period$variable]]>length]
            }
            if (NROW(d)>0){
                ## filter
                if (!is.null(search.term)){
                    d <- d[grepl(search.term,d[[var]],...),.SD,.SDcols=c(by,var,sortkey)]
                }
                ## select
                if (NROW(d)>0){
                    d <- switch(select,
                                "first"={
                                    setorderv(d,c(by,sortkey),order=c(1,1))
                                    d[d[,.I[1],by=c(by)]$V1,c(var,sortkey),with=FALSE]
                                    if (length(name)==1)
                                        setnames(d,c(var,sortkey),paste0(name,c("",".date")))
                                    else if (length(name)==length(c(var,sortkey)))
                                        setnames(d,c(var,sortkey),name)
                                },
                                "last"= {
                                    ## sort ascending so that largest date is first
                                    setorderv(d,c(by,sortkey),order=c(1,-1))
                                    d[d[,.I[.N],by=c(by,sortkey)]$V1]
                                    if (length(name)==1)
                                        setnames(d,c(var,sortkey),paste0(name,c("",".date")))
                                    else if  (length(name)==length(c(var,sortkey)))
                                        setnames(d,c(var,sortkey),name)
                                },
                                "atleast2diff"= {
                                    ## setorder(d,by,sortkey)
                                    d[,newvariable=length(unique(var)),by=c(by)]
                                    setnames(d,"newvariable",name)
                                },
                                "unique.pnr"={
                                    unique(d[,.(pnr)])
                                },
                                {
                                    if (is.null(collect))
                                        d
                                    else {
                                        d <- d[,unique(c(collect,"pnr")),with=FALSE]
                                        if (length(unique(c(name,"pnr")))==length(names(d)))
                                            setnames(d,unique(c(name,"pnr")))
                                        d
                                    }
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
                                  select=select,
                                  name=name,collect=collect), 
                             ...)
    return(f)
}
    
##' Pre-processing data
##'
##' Pre-processing data according to instructions in dpp object
##' @title Data pre-processing 
##' @param x dpp object
##' @param n number of rows from each subject
##' @param verbose bla bla?
##' @param ... not (yet used)
##' @export 
process <- function(x,n=Inf,verbose=TRUE,...){
    if (!is.infinite(n)){
        register.environment <- lapply(x$regis,function(d){
            d[1:n]
        })
    }else{
        register.environment <- x$regis
    }
    ## inclusion
    for (i in 1:length(x$inclusion)){
        Inc <- x$inclusion[[i]]
        Inc.study <- do.call(Inc,c(attr(Inc,"arguments"),list(environment=register.environment)))
        if (is.null(Inc.study)){
            message(paste0("\nWell, for some reason this search did not match any subject.\nPlease investigate the particularities of your inclusion criteri",ifelse(length(x$inclusion)>1,"a.","on."),"\n"))
        }else{
            message(paste0("\n",sample(c("Nice","Coolio","Well done","Wow","Not bad"),size=1),
                           ", your inclusion criterion '", names(x$inclusion)[[i]],"' matched ",NROW(Inc.study)," subjects."))
        }
        if (length(x$study)>0)
            x$study <- rbindlist(list(x$study,Inc.study))
        else
            x$study <- Inc.study
    }
    ## restrict all further operations to study population
    register.environment <- lapply(x$regis,function(d){
        setkey(d,pnr)
        d[x$study]
    })
    ## exclusion
    for (e in 1:length(x$exclusion)){
        Ex <- x$exclusion[[e]]
        Ex.study <- do.call(Ex,c(attr(Ex,"arguments"),list(environment=register.environment)))
        ## browser()
        if (NROW(Ex.study)>0){
            before.n <- NROW(x$study)
            x$study <- x$study[!(pnr%in%Ex.study$pnr)]
            after.n <- NROW(x$study)
            if (after.n<before.n){
                message(paste0(sample(c("Reeeejected","Nuuuullified","Remoooooved","Kicked outoutout","Gone forever","Send to a device called null were"),size=1),
                               " ",before.n-after.n," subjects based on exclusion criterion: '",names(x$exclusion)[[e]],"'."))
            }
        }
    }
    ## baseline
    for (v in 1:length(x$variable)){
        message("Processing instructions for variable: ",names(x$variable)[[v]])
        V <- x$variable[[v]]
        new.V <- do.call(V$instructions,c(attr(V$instructions,"arguments"),list(environment=register.environment)))
        if (NROW(new.V)>0){
            setkey(new.V,pnr)
            if (V$target=="fup"){
                x[["fup"]] <- rbindlist(list(x[["fup"]],new.V),fill=TRUE)
            }else{
                if (is.null(x[[V$target]])){
                    x[[V$target]] <- new.V
                }else{
                    x[[V$target]] <- merge(x[[V$target]],new.V,by="pnr")
                }
            }
        }
    }
    ## followup
    x
}

######################################################################
### dpp.R ends here
