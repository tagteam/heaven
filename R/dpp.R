### dpp.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Oct 17 2018 (13:53) 
## Version: 
## Last-Updated: Jun 27 2019 (14:41) 
##           By: Thomas Alexander Gerds
##     Update #: 239
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
##' @examples
##' \dontrun{
##' library(heaven)
##' library(data.table)
##' 
##' dc <- contentSAS("x:/Data/Rawdata_Hurtig/706818/diag_indl.sas7bdat")
##' d2 <- contentSAS("x:/Data/Rawdata_Hurtig/706818/lmdb.sas7bdat")
##' # dll <- contentSAS("x:/Data/Rawdata_Hurtig/706818/labka.sas7bdat",obs=10)
##' pp <- contentSAS("x:/Data/Rawdata_Hurtig/706818/pop.sas7bdat")
##' pp10 <- importSAS("x:/Data/Rawdata_Hurtig/706818/pop.sas7bdat",obs=10,date.vars="FdaTo")
##' 
##' ddd <- importSAS("x:/Data/Rawdata_Hurtig/706818/doede.sas7bdat",
##'        keep=c("pnr","doddato"),obs=10,date.vars="doddato")
##' 
##' ## d <- importSAS("x:/Data/Rawdata_Hurtig/706818/diag_indl.sas7bdat",
##' obs=100,where="pnr ne ''",keep=c("pnr","uddto"))
##' 
##' source("v:/Data/Workdata/706818/ThomasAlexanderGerds/testheaven/R/new-process.R")
##' setwd("v:/Data/Workdata/706818/ThomasAlexanderGerds/testheaven/")
##' x <- dpp()
#' addData(x,
#'         name="lmdb",
#'         variables=c("atc","date"="eksd")) <- "x:/Data/Rawdata_Hurtig/706818/lmdb.sas7bdat"
#' addData(x,
#'         name="pop",
#'         variables=c("sex","date"="fdato")) <- "x:/Data/Rawdata_Hurtig/706818/pop.sas7bdat"
#' addData(x,
#'         name="dod",
#'         variables=c("date"="doddato")) <- "x:/Data/Rawdata_Hurtig/706818/doede.sas7bdat"
#' addData(x,
#'         name="lpr",
#'         variables=c("diag",
#'                     "diagtype",
#'                     "date"="inddto",
#'                     "date"="uddto",
#'                     "pattype")) <- "x:/Data/Rawdata_Hurtig/706818/diag_indl.sas7bdat"
#' addInclusion(x,"firstAF") <- importer("lpr",
#'                                       keep=c("pnr","diag","inddto","uddto","pattype"),
#'                                       where="inddto >= '01jan97'd and (diag contains 'DI48' or
#'  substr(diag,1,5) in ('42793' '42794')) and pnr ne ' ' and pattype ne 3",
#'                                       select="unique")
#' addExclusion(x,"earlyAF") <- importer("lpr",
#'                                       keep=c("pnr","diag","inddto","uddto","pattype"),
#'                                       where="inddto < '01jan97'd and (diag contains
#'  'DI48' or substr(diag,1,5) in ('42793' '42794')) and pnr ne ' ' and pattype ne 3",
#'                                       select="unique")
#' ## start followup
#' addVariable(x,target="afbsl") <- selector(data="studypop",
#'                                           var="afstart7",
#'                                           name="afstart7",
#'                                           sortkey=NULL,
#'                                           search.term=NULL,
#'                                           select="expression",
#'                                           collect=expression(uddto+7))
#' ## end followup
#' addVariable(x,target="studypop") <- selector(data="dod",
#'                                              var="efup.date",
#'                                              name="efup.date",
#'                                              sortkey=NULL,
#'                                              search.term=NULL,
#'                                              select="expression",
#'                                              collect=expression(pmin(doddato,
#'                                                                      as.Date("2017-01-01"),
#'                                                                      na.rm=TRUE)))
#' addVariable(x,target="afbsl") <- selector(data="pop",
#'                                           var="sex",
#'                                           name="sex",
#'                                           sortkey=NULL,
#'                                           search.term=NULL,
#'                                           select="expression",
#'                                           collect=expression(factor(sex,
#'                                                                     levels=c(0,1),
#'                                                                     labels=c("Female","Male"))))
#' addVariable(x,target="afbsl") <- selector(data="pop",
#'                                           var="fdato",
#'                                           name="birthday",
#'                                           sortkey=NULL,
#'                                           search.term=NULL,
#'                                           select="variable",
#'                                           collect="fdato")
#' addVariable(x,
#'             target="afbsl") <- selector(data="afbsl",
#'                                         var="age_af7",
#'                                         sortkey=NULL,
#'                                         search.term=NULL,
#'                                         select="expression",
#'                                         collect=expression(round(as.numeric(afstart7
#'                                                                             -birthday)/365.25,1)))
#' addVariable(x,target="fup",type="rbind") <- selector(data="lmdb",
#'                                                      var="atc",
#'                                                      name="drug",
#'                                                      sortkey="eksd",
#'                                                      search.term="B01A")
#' addVariable(x,target="fup",type="rbind") <- selector(data="lpr",
#'                                                      var="diag",
#'                                                      name="stroke",
#'                                                      sortkey="inddto",
#'                                                      search.term="I63")
#' 
##' 
##' source("v:/Data/Workdata/706818/ThomasAlexanderGerds/testheaven/R/new-process.R")
##' x <- process(x,n=1000,fish=10000,show.sas.code=0L,restart="all")
##' 
##' x0$data$afbsl
##' x0$data$fup
##' 
##' 
##' ## x <- process(x,n=Inf,fish=Inf,show.sas.code=0L)
##' 
##' 
##' x1 <- process(x,n=1000,fish=Inf,show.sas.code=0L)
##' source("v:/Data/Workdata/706818/ThomasAlexanderGerds/testheaven/R/new-process.R")
##' 
##' 
##' ## if substr(atc,1,7) in ('B01AA03','B01AA04') then output vka;
##' ## if substr(atc,1,7) in ('B01AE07','B01AF01','B01AF02','B01AF03') then output noac;
##' ## if substr(atc,1,7) in ('B01AE07') then output dabi;
##' ## if substr(atc,1,7) in ('B01AF01') then output riva;
##' ## if substr(atc,1,7) in ('B01AF02') then output apix;
##' ## if substr(atc,1,7) in ('B01AF03') then output edox;
##' 
##' source("v:/Data/Workdata/706818/ThomasAlexanderGerds/testheaven/R/new-process.R")
##' x1 <- process(x,n=1000,fish=Inf,show.sas.code=0L)
##' 
##' x1 <- process(x1,n=1000,fish=Inf,show.sas.code=0L)
##' 
##' 
##' system.time(x <- process(x,n=Inf,fish=Inf,show.sas.code=0L))
##' ## x <- process(x,n=30,fish=100000,show.sas.code=0L,restart="age")
##' ## system.time(x <- process(x,n=1000,fish=Inf,show.sas.code=0L))
##' system.time(x <- process(x,n=Inf,fish=Inf,show.sas.code=0L))
##' print(object.size(x),units="Mb")
##' }
##' @export
##' @param id Character. Name of subject id variable. Default is \code{"pnr"}
dpp <- function (id = "pnr") {
    x = structure(list(rawdata = list(),
                       data = list(),
                       variables = list(),
                       inclusion = list(),
                       exclusion = list(),
                       info = list(id = id, n= 0)),
                  class = "dpp")
    return(x)
}

##' @export
print.dpp <- function (x, ...) {
    if ((lenwork <- length(x$data$studypop)) > 0) {
        cat("\nStudy population according to inclusion/exclusion criteria:\n",
            "$studypop contains", NROW(x$data$studypop), " subjects.")
    }
    else {
        cat("\nEmpty study population.\n", ifelse(length(x$inclusion) >
            0, "r", "You should define at least one inclusion criterion and then r"),
            "un process(x) to change this.\n", sep = "")
    }
    if ((len.rawdata <- length(x$rawdata)) > 0) {
        cat("\nChecked in ", len.rawdata, " registers at $rawdata:\n",
            sep = "")
        lapply(1:len.rawdata, function(i) {
            cat(" + ", names(x$rawdata)[i], ":", sep = "")
            if ((nn <- NROW(x$rawdata[[i]]$dt)) == 0)
                cat(" ", x$rawdata[[i]]$file, ", Variables:",
                  sep = "")
            else cat(" Rows: ", nn, ", Variables:", sep = "")
            cat(" ", paste(x$rawdata[[i]]$variables, collapse = ", "),
                "\n", sep = "")
        })
    }
    if ((len.bsl <- length(x$data)) > 0) {
        cat("\nExtracted ", len.bsl, ifelse(len.bsl == 1, " dataset",
            " datasets"), ":\n", sep = "")
        for (dd in names(x$data)) {
            cc <- colnames(x$data[[dd]])
            cc <- if (length(cc) < 5) {
                paste(cc, collapse = ", ")
            }
            else {
                paste0(paste(cc[1:4], collapse = ", "), " ..., ",
                  cc[[length(cc)]])
            }
            cat(" + $data$", dd, ": ", NROW(x$data[[dd]]), " rows and ",
                NCOL(x$data[[dd]]), " columns(", cc, ")", "\n",
                sep = "")
        }
    }
}

##' import data from a register
##'
##' import data from a register
##' @title import data from a register
##' @param data names of data
##' @param residents only residents?
##' @param where SAS where statement 
##' @param keep which variables to keep
##' @param select select subset. currently only \code{"unique"} is supported
##' @param ... passed to importSAS
##' @return data.table
##' @seealso dpp
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
importer <- function(data,
                     residents=TRUE,
                     where,
                     keep="pnr",
                     select="unique",
                     ...){
    .I=NULL
    if (!missing(where)){
        if (length(keep[keep!="pnr"])==0){
            stop("Please specify the variables that should enter the sas 'where' statement in argument keep.")
        }
    }
    if (missing(data)) {
        stop(paste0(sample(c("Sorry", "Honey", "Sweety", "Attagirl",
                             "Attaboy", "My dear"), size = 1), ", we cannot search thin air. Need data to work with."))
    }
    else {
        if (!is.character(data)) {
            stop(paste0(sample(c("No, no,no. I told you that", "Looking at the help page shows that",
                                 "Reading the docs would reveal that", "You forgot that",
                                 "Nice try, but"), size = 1), " data has\nto be the name of a data set, i.e., a character."))
        }
    }
    f <- function(data,where,keep,select,residents,...,obs,environment){
        env <- environment[[data]]
        ia <- c(list(filename=env$file,obs=obs,where=where,keep=keep,verbose=FALSE),
                list(...))
        ia$show.sas.code=FALSE
        ## if (length(ia$colClasses)>0){
            ## if (match("character",names(ia$colClasses),nomatch=0L))
                ## ia$colClasses[["character"]] <- unique(c("pnr",ia$colClasses[["character"]]))
        ## }else{
            ## ia$colClasses <- list("character"="PNR")
        ## }
        tryf <- try(d <- do.call("importSAS",ia))
        if (class(tryf)[1]=="try-error"){
            ia$show.sas.code=TRUE
            tryf <- try(do.call("importSAS",c(ia,list(save.tmp=TRUE))))
        }
        data.table::setnames(d,tolower(names(d)))
        if(residents==TRUE){
            ## d[,pnr:=as.numeric(pnr)]
            ## d <- d[!is.na(pnr)]
            ## FIXME: remove empty pnr
        }
        out <- switch(select,"unique"= d[d[,.I[1],by="pnr"]$V1],d)
        out
    }
    attr(f, "arguments") <- c(list(data=data,
                                   where=where,
                                   keep=keep,
                                   select=select,residents=residents),
                              list(...))
    return(f)
}

##' Selecting and collecting information from raw data to processed data
##'
##' This is the master function for several different data processing tasks
##' @title Selecting and collecting information from raw data
##' @param data Name of raw data source
##' @param var raw data variables
##' @param name name of this action
##' @param varnames names for processed variable
##' @param by by variable
##' @param search.term search term applied to var
##' @param search.expression search expression applied to var
##' @param sortkey in this order
##' @param period list with three elements: variable, start, stop
##' @param backward search backward in time
##' @param forward search forward in time
##' @param select what to select 
##' @param collect what to collect
##' @param ... additional arguments
##' @author Thomas Alexander Gerds tag@@biostat.ku.dk
##' @export
selector <- function(data, var, name, varnames, by = "pnr", search.term = NULL,
    search.expression = NULL, sortkey = NULL, period = NULL,
    backward = NULL, forward = NULL, select = "first", collect = NULL,
    ...)
{
    .SD = .I = .N = pnr = NULL
    if (missing(data)) {
        stop(paste0(sample(c("Sorry", "Honey", "Sweety", "Attagirl",
            "Attaboy", "My dear"), size = 1), ", we cannot search thin air. Need data to work with."))
    }
    else {
        if (!is.character(data)) {
            stop(paste0(sample(c("No, no,no. I told you that",
                "Looking at the help page shows that", "Reading the docs would reveal that",
                "You forgot that", "Nice try, but"), size = 1),
                " data has\nto be the name of a data set, i.e., a character."))
        }
    }
    if (missing(name))
        name <- var
    if (missing(varnames))
        varnames <- name
    f <- function(data, var, by, search.term, search.expression,
        sortkey, period, backward, forward, select, name, collect,
        ..., environment) {
        d <- environment[[data]]
        if (length(d) == 0) {
            stop(paste0(sample(c("Very sorry, but", "This is messed up somehow,",
                "Damn it,", "May I kindly remind you that", "Impossible to proceed, because"),
                size = 1), " data ", data, " has not been checked in yet.\n",
                ifelse(length(names(environment)) > 0, paste0("Checked in data are: ",
                  paste(names(environment), collapse = ", ")),
                  "")))
        }
        if (NROW(d) > 0) {
            if (!missing(period) && !is.null(period)) {
                if (is.null(period$stop)) {
                  if (is.null(period$start)) {
                  }
                  else {
                    d <- d[(d[[period$variable]] >= period$start)]
                  }
                }
                else {
                  if (is.null(period$start)) {
                    d <- d[(d[[period$variable]] <= period$stop)]
                  }
                  else {
                    d <- d[(d[[period$variable]] >= period$start) &
                      (d[[period$variable]] <= period$stop)]
                  }
                }
            }
            if (!missing(backward) && !is.null(backward)) {
                if (is.null(backward$data)) {
                  bd = "study"
                }
                bd = eval(as.name(backward$data))
                if (!(by %in% names(bd))) {
                  stop(paste0(sample(c("Oooh", "Uups", "Beginner mistake",
                    "No no no no no", "Try again"), size = 1),
                    ", the variable ", by, " is not in reference data for backward search."))
                }
                setkeyv(bd, by)
                setkeyv(d, by)
                if (backward$reference %in% names(d)) {
                  setnames(bd, backward$reference, "canttouchthis")
                  backward$reference <- "canttouchthis"
                }
                d <- bd[d, data.table::data.table(by, backward$reference)]
                d <- d[d[[backward$reference]] - d[[period$variable]] >
                  length]
            }
            if (NROW(d) > 0) {
                if (!is.null(search.term)) {
                  d <- d[grepl(search.term, d[[var]]), .SD, .SDcols = c(by,
                    var, sortkey)]
                }
                if (!is.null(search.expression)) {
                  d <- d[eval(search.expression), .SD, .SDcols = c(by,
                    var, sortkey)]
                }
                if (NROW(d) > 0) {
                  d <- switch(select, first = {
                    data.table::setorderv(d, c(by, sortkey),
                                          order = c(1, 1))
                    d <- d[d[, .I[1], by = c(by)]$V1, c(by, var, sortkey),
                      with = FALSE]
                    if (length(varnames) == 1) setnames(d, c(var,
                      sortkey), paste0(varnames, c("", ".date"))) else if (length(varnames) ==
                      length(c(var, sortkey))) setnames(d, c(var,
                      sortkey), varnames)
                  }, last = {
                    data.table::setorderv(d, c(by, sortkey),
                      order = c(1, -1))
                    d[d[, .I[.N], by = c(by, sortkey)]$V1,c(by,var,sortkey)]
                    if (length(varnames) == 1) {
                      setnames(d, c(var, sortkey), paste0(varnames,
                        c("", ".date")))
                    } else {
                      if (length(varnames) == length(c(var, sortkey))) setnames(d,
                        c(var, sortkey), varnames)
                    }
                  }, atleast2diff = {
                    d[, newvariable = length(unique(var)), by = c(by)]
                    setnames(d, "newvariable", varnames)
                  }, unique.pnr = {
                    unique(d[, data.table(pnr)])
                  }, expression = {
                    d <- d[, data.table(eval(collect), pnr)]
                    setnames(d, unique(c(varnames, "pnr")))
                    d
                  }, {
                    if (is.null(collect)) d else {
                      d <- d[, unique(c(collect, "pnr")), with = FALSE]
                      if (length(unique(c(varnames, "pnr"))) ==
                        length(names(d))) setnames(d, unique(c(varnames,
                        "pnr")))
                      d
                    }
                  })
                }
            }
        }
        return(d)
    }
    attr(f, "arguments") <- c(list(data = data, var = var, name = name,
        varnames = varnames, by = by, search.term = search.term,
        search.expression = search.expression, sortkey = sortkey,
        period = period, backward = backward, forward = forward,
        select = select, collect = collect), list(...))
    return(f)
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
                        name,
                        id = "pnr",
                        variables = NULL,
                        ...,
                        value)
{
    stopifnot(class(x) == "dpp")
    cl <- class(value)[1]
    if (cl == "data.frame") {
        value <- data.table(value)
        cl <- "data.table"
    }
    if (missing(name)){
        stop(paste0("In order to refer to this dataset later on we need to agree upon a name.\n",
                    "A good name is short has no strange or difficult to type characters, has no space characters,\n",
                    "and is still informative. Good names are lmdb, lpr, pop, opr. In this call you\n",
                    "forgot to provide a name for your dataset. Please try again ..."))
    }else{
        if (name %in% names(x$rawdata))
            warning(paste0(sample(c("Uuups. ","Olalala. ","Note from your preprocessing assistant. "),size=1),"Replacing an already checked in dataset with the same name.\n"))
        data.names <- sapply(x$rawdata,function(r)r$object.name)
        if ((this <- as.character(substitute(value))) %in% data.names)
            warning(paste0(sample(c("Not sure if ","Unclear if ","Wonder if "),size=1),"you on purpose check in the same data again?\n",
                           "There is already a dataset in x$rawdata\nwhich looks very much (exactly) like:\n",this))
    }
    value <- switch(cl,
                    data.table = {
                        setnames(value,tolower(names(value)))
                        if (match(tolower(id), names(value), nomatch = 0) ==0)
                            stop(paste0(sample(c("Hmm, ","Yo, ","Lady, ","Dude, "),size=1),
                                        " cannot see the subject identification variable ",
                                        id,
                                        " among the variables. Halting."))
                        if (!is.null(variables))
                            rawdata <- list(list(dt=value[, c(variables, id), with = FALSE],
                                                 object.name=this,
                                                 done=TRUE,
                                                 variables=variables,
                                                 origin="data.table",
                                                 file=NA))
                        else {
                            if (id != "pnr") setnames(value, id, "pnr")
                            rawdata <- list(list(dt=value,
                                                 object.name=this,
                                                 variables=variables,
                                                 n=TRUE,
                                                 origin="data.table",
                                                 file=NA))
                        }
                        names(rawdata) <- name
                    }, character = {
                        if (file.exists(value)) {
                            rawdata <- list(list(dt=NULL,
                                                 object.name=this,
                                                 variables = variables,
                                                 done=FALSE,
                                                 origin="file.name",
                                                 file = value))
                            names(rawdata) <- name
                        } else {
                            stop(paste0("File does not exist: ", value))
                        }
                    }, {
                        stop(paste0("Don't know how to read this object. It has class : ", cl))
                    })
    x$rawdata <- c(rawdata, x$rawdata)
    return(x)
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
##' @param target Character. The name of a baseline date or \code{"fup"} for the followup dataset. Where to store the result.
##' @param data Optional the name of the data table in which to evaluate \code{value}.
##' @param type Character that determines how to add the variable to the data set. One of \code{"merge"}, \code{"join"} or \code{"rbind"}
##' @param depends vector of names of other instructions (inclusion, exclusion, variables) that should be evaluated before this one.
##' @param priority Processing priority. Maybe changed by \code{depends}.
##' @param by Character vector. Specifying subsets in which \code{value} is evaluated. E.g., if set to \code{"pnr"} then \code{value} is applied to the data of each subject separately. 
##' @param ... not (yet) used
##' @param value A function obtained with the all-purpose function \code{selector} or one of the specialized functions. See \code{dpp}.
##' @export
"addVariable<-" <- function(x,
                            target,
                            data,
                            type=c("merge","join","rbind"),
                            depends = NULL,
                            priority = c("late","early"),
                            by = NULL,
                            ...,
                            value){
    type = match.arg(tolower(type),c("merge","join","rbind"))
    priority = match.arg(tolower(priority),c("late","early"))
    stopifnot(is.function(value) | class(value)[1] == "formula")
    varname <- attr(value,"arguments")$name
    if (is.null(varname)) stop("Sigh. Of course, a variable needs a name.")
    var <- list(list(instructions = value, target = target,
                     type=type,sources=attr(value,"arguments")$data,
                     name=varname,
                     done=FALSE,
                     depends = depends,
                     priority = priority, by = by))
    names(var) <- varname
    if (varname %in% names(x$variable))
        x$variable[[varname]] <- var[[1]]
    else
        x$variable <- c(x$variable, var)
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
"addInclusion<-" <- function (x, name, target = "all",
                              priority = c("late", "early"),
                              ..., value)
{
    priority = match.arg(priority)
    if (name %in% names(x$inclusion)){
        x$inclusion[[name]] <- value
    }else{
        inc <- list(value)
        names(inc) <- name
        if (priority == "early") {
            x$inclusion <- c(inc, x$inclusion)
        }
        else {
            x$inclusion <- c(x$inclusion, inc)
        }
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
##' @export
'addExclusion<-' <- function(x, name, target = "all", priority = c("late", "early"),
                              ..., value)
{
    priority = match.arg(priority)
    if (name %in% names(x$exclusion)){
        x$exclusion[[name]] <- value
    }else{
        excl <- list(value)
        names(excl) <- name
        if (priority == "early") {
            x$exclusion <- c(excl, x$exclusion)
        }
        else {
            x$exclusion <- c(x$exclusion, excl)
        }
    }
    return(x)
}


##' read raw data
##'
##' read raw data
##' @title read raw data
##' @param x dpp object
##' @param id id variable
##' @param n sample size
##' @param ...  passed to subroutine: fread or importSAS
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
getRawdata <- function(x, id, n, ...){
    ## FIXME: deal with origin!=filename
    if (x$origin=="file.name"){
        if (!file.exists(x$file)){
            stop(paste0("Hov. Previously existing file ",x$file," has disappeared."))
        }
        ext <- tools::file_ext(x$file)
        rawdata <- switch(ext, sas7bdat = {
            ia <- c(list(filename = x$file,
                         obs=n,
                         keep=unique(c(id, x$variables))),
                    list(...))
            date.vars <- x$variables[match("date",tolower(names(x$variables)),nomatch=0)]
            if(length(date.vars)>0)
                ia <- c(ia,list(date.vars=date.vars,verbose=FALSE))
            ## if (length(ia$colClasses)>0){
                ## if (match("character",names(ia$colClasses),nomatch=0L))
                    ## ia$colClasses[["character"]] <- unique(c("pnr",ia$colClasses[["character"]]))
            ## }else{
                ## ia$colClasses <- list("character"="PNR")
            ## }
            x$dt <- do.call("importSAS",ia)
        }, csv = {
            line1 <- do.call(data.table::fread, list(file = x$file,
                                                     nrows = 2, ...))
            if (length(x$variables) == 0) {
                cc <- rep("character", NCOL(line1))
            } else {
                cc <- rep("NULL", NCOL(line1))
            }
            names(cc) <- names(line1)
            rm(line1)
            cc[grep(unique(c(id, x$variables)), names(cc))] <- "character"
            x$dt <- do.call(data.table::fread,
                            list(file = x$file,
                                 nrows = n, colClasses = cc, ...))
        })
        data.table::setkeyv(x$dt,id)
        return(x)
    }
}
    
##' Pre-processing data
##'
##' Pre-processing data according to instructions in dpp object
##' @title Data pre-processing
##' @param x dpp object
##' @param n sample size for all inclusion criteria
##' @param fish sample size for raw data (registers)
##' @param verbose bla bla?
##' @param show.sas.code show sas code
##' @param restart string indicating which for which terms the processing should be restarted. set to \code{"all"} to restart everything.
##' @param ... not (yet used)
##' @export
##'
##' 
process <- function (x, n = Inf, fish = "studypop", verbose = TRUE, show.sas.code = FALSE,
                     restart = FALSE, ...)
{
    pnr = NULL
    restart <- tolower(restart)
    if ("studypop" %in% restart)
        restart <- "all"
    if (restart == "all" || "variables" %in% restart) {
        x$data <- list(studypop=x$data$studypop)
    }
    len.variable <- length(x$variable)
    if (len.variable > 0) {
        vtargets <- sapply(x$variable, function(v) v$target)
        vtype <- sapply(x$variable, function(v) v$type)
        vrestart <- names(x$variabe) %in% restart
        restart <- c(restart, tolower(unique(vtargets[vrestart &
                                                      vtype == "rbind"])))
    }
    for (r in restart) {
        if (tolower(r) %in% names(x$data))
            x$data[[r]] <- NULL
    }
    message("\n===============================================================",
            "\nData preprocessing step 1a: inclusion", "\n===============================================================")
    xn <- x$info$n
    if ((len.inclusion <- length(x$inclusion)) > 0) {
        if ((xn < n) || restart == "all") {
            x$data$studypop <- NULL
            restart <- "all"
            message("\nApplying inclusion ", ifelse(len.inclusion ==
                                                    1, paste0("criterion: ", names(x$inclusion),
                                                              "\n"), paste0("criteria: ", paste(names(x$inclusion),
                                                                                                collapse = "\n-"))))
            for (i in 1:len.inclusion) {
                Inc <- x$inclusion[[i]]
                Inc.study <- do.call(Inc, c(attr(Inc, "arguments"),
                                            list(obs = n, environment = x$rawdata)))
                if (is.null(Inc.study) || NROW(Inc.study) ==
                    0) {
                    message("Well, for some reason this search did not match any subject.\nPlease investigate the particularities of your inclusion criterion",
                            ifelse(length(x$inclusion) > 1, "a.", "on."))
                }
                else {
                    message(sample(c("Nice", "Coolio", "Well done",
                                     "Wow", "Not bad"), size = 1), ", your inclusion criterion '",
                            names(x$inclusion)[[i]], "' matched ", NROW(Inc.study),
                            " subjects.")
                }
                if (length(x$data$studypop) > 0)
                    x$data$studypop <- rbindlist(list(x$data$studypop,
                                                      Inc.study), fill = TRUE)
                else x$data$studypop <- Inc.study
            }
            x$info$n <- n
        }
        else {
            if (xn > n) {
                message(paste0("\nSubsetting previously extracted study population."))
                x$data$studypop <- x$data$studypop[1:n]
                x$info$n <- n
            }
            else {
                message(paste0("\nUsing previously extracted study population.\nCheck in as restart=TRUE to change this."))
            }
        }
    }
    else {
        stop(paste0(sample(c("Oh my gosh, please focus.", "Sorry, but are you new to this?",
                             "This is so obvious.", "Sorry, no inclusion criterion, no data."),
                           size = 1), " ", "Need at least one inclusion criterion."))
    }
    if ((len.exclusion <- length(x$exclusion)) > 0) {
        for (e in 1:len.exclusion) {
            if (restart == "all" || "exclusion" %in% restart) {
                message("\n===============================================================",
                        "\nData preprocessing step 1b: exclusion",
                        "\n===============================================================")
                message("\nApplying exclusion ", ifelse(len.exclusion ==
                                                        1, paste0("criterion: ", names(x$exclusion),
                                                                  "\n"), paste0("criteria: ", paste(names(x$exclusion),
                                                                                                    collapse = "\n-"))))
                Ex <- x$exclusion[[e]]
                Ex.study <- do.call(Ex, c(attr(Ex, "arguments"),
                                          list(obs = n, environment = x$rawdata)))
                if (NROW(Ex.study) > 0) {
                    before.n <- NROW(x$data$studypop)
                    x$data$studypop <- x$data$studypop[!(pnr %in% Ex.study$pnr)]
                    after.n <- NROW(x$data$studypop)
                    if (after.n < before.n) {
                        message(paste0(sample(c("Reeeejected", "Nuuuullified",
                                                "Remoooooved", "Kicked outoutout", "Gone forever",
                                                "Send to a device called null were"), size = 1),
                                       " ", before.n - after.n, " subjects based on exclusion criterion: '",
                                       names(x$exclusion)[[e]], "'."))
                    }
                }
            }
        }
    }

    setkey(x$data$studypop, pnr)
    study.pnr <- x$data$studypop[["pnr"]]
    where.id <- paste0("pnr in (", paste0("'", study.pnr, "'",
                                          collapse = "\n"), ")")
    message("\n===============================================================",
            "\nData preprocessing step 2: extracting raw data from registers",
            "\n===============================================================\n")
    rawdata.environment <- NULL
    redone <- NULL
    for (r in (1:length(x$rawdata))) {
        d <- x$rawdata[[r]]
        name.d <- names(x$rawdata)[[r]]
        if ((restart != "all") && !(name.d %in% restart) && d$done == fish) {
            if (xn > n) {
                message(paste0("Subsetting previously extracted data from ",
                               name.d, "."))
                if (is.infinite(fish)) {
                    x$rawdata[[r]]$dt <- x$rawdata[[r]]$dt[x$data$studypop]
                }
                else {
                    x$rawdata[[r]]$dt <- x$rawdata[[r]]$dt[1:n]
                    x$rawdata[[r]]$done <- n
                }
                redone <- unique(c(redone, names(x$rawdata)[[r]]))
            }
            else {
                message(paste0("Using previously extracted data from ",
                               name.d, "."))
            }
        }
        else {
            redone <- unique(c(redone, name.d))
            if (fish == "studypop" || is.infinite(fish)) {
                if (verbose) {
                    message(paste0("Reading all data lines in ",
                                   "", name.d, " register corresponding to ",
                                   length(study.pnr), " study subjects", "... be patient, this may take some time.\n"))
                }
                d <- getRawdata(d, id = x$info$id, n = Inf, show.sas.code = show.sas.code,
                                where = where.id, ...)
                if (verbose) {
                    message("Read ", NROW(d$dt), " data lines from ",
                            name.d, " register.")
                }
            }
            else {
                if (verbose) {
                    message(paste0("Reading first ", fish, " data lines in ",
                                   "", name.d, " register, and filter those corresponding to one of the ",
                                   length(study.pnr), " study subjects ...\n",
                                   "... be patient, this may take some time.\n"))
                }
                d <- getRawdata(d, id = x$info$id, n = fish,
                                show.sas.code = show.sas.code, ...)
                setkey(d$dt, pnr)
                d$dt <- d$dt[x$data$studypop[, pnr]]
                if (verbose) {
                    message("Found ", NROW(d$dt), " matching data lines in ",
                            name.d, " register.")
                }
            }
            x$rawdata[[r]]$dt <- d$dt
            x$rawdata[[r]]$done <- fish
        }
        rawdata.environment[[r]] <- x$rawdata[[r]]$dt
        names(rawdata.environment)[[r]] <- names(x$rawdata)[[r]]
    }
    rawdata.environment <- c(rawdata.environment, x$data)
    message("\n===============================================================\n",
            "Step 3: Processing variable instructions: ", "\n===============================================================\n")
    if (len.variable > 0) {
        for (v in 1:len.variable) {
            V <- x$variable[[v]]
            Vname <- names(x$variable)[[v]]
            if (V$done == FALSE || restart == "all" || V$target %in%
                restart || "variables" %in% tolower(restart) ||
                tolower(Vname) %in% restart || any(V$sources %in%
                                                   redone)) {
                # reset existing elements
                if (Vname %in% names(x$data[[V$target]])) {
                    if (any(V$sources %in% redone) || "variables"%in%restart || tolower(Vname) %in%
                        restart) {
                        x$data[[V$target]][[Vname]] <- NULL
                        if (match(paste0(Vname, ".date"), names(x$data[[V$target]]),
                                  nomatch = 0))
                            x$data[[V$target]][[paste0(Vname, ".date")]] <- NULL
                        if (NCOL(x$data[[V$target]]) == 1 && names(x$data[[V$target]]) ==
                            "pnr")
                            x$data[[V$target]] <- NULL
                    }
                    else {
                        stop("Variable ", Vname, " exists in dataset ",
                             V$target)
                    }
                }
                # announce operation
                message(paste0(paste(V$sources, collapse = ", "),
                               " -> ", V$target, "$", Vname))
                # apply variable instructions
                try.V <- try(new.V <- do.call(V$instructions,
                                              c(attr(V$instructions, "arguments"), list(environment = rawdata.environment))),
                             silent = FALSE)
                if ("try-error" %in% class(try.V)) {
                    message(paste0("Problems with processing variable ",
                                   Vname))
                    new.V <- NULL
                }
                # add data to target
                if (NROW(new.V) > 0) {
                    data.table::setkey(new.V, pnr)
                    if (length(x$data[[V$target]]) == 0) {
                        # this is first variable in target
                        x$data[[V$target]] <- new.V
                    }
                    else {
                        ## browser(skipCalls=1L)
                        switch(V$type, merge = {
                            x$data[[V$target]] <- merge(x$data[[V$target]],
                                                        new.V, by = "pnr")
                        }, join = {
                            orig.names <- names(x$data[[V$target]])
                            new.names <- names(new.V)
                            x$data[[V$target]] <- new.V[x$data[[V$target]]]
                            setcolorder(x$data[[V$target]],union(orig.names,new.names))
                        }, rbind = {
                            x$data[[V$target]] <- rbindlist(list(x$data[[V$target]],
                                                                 new.V), fill = TRUE)
                        })
                        rawdata.environment[[V$target]] <- x$data[[V$target]]
                    }
                }
                x$variable[[v]]$done <- TRUE
            }
            else {
                message("Variable ", Vname, " as processed previously.")
            }
        }
    }
    else {
        cat("\nNo variables checked in yet. Do this via 'addVariable'.\n")
    }
    x
}


##' Add a drug to an existing dpp object 
##'
##' Add a drug to an existing dpp object 
##' @title Add drug
##' @param x dpp object
##' @param target name of dataset 
##' @param drug character string which should match \code{names(diseasecode)}
##' @param ... not used
##' @return updated object
##' @seealso addSex, addAge, addVariable, addComo
##' @examples # see help(dpp)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
addDrug <- function(x,target,drug,...){
    ## extraction of drug
    utils::data(diseasecode)
    drug.match <- grep(drug,names(diseasecode),ignore.case=TRUE,value=TRUE)
    if (length(drug.match)==0) stop("Cannot find definition of drug ",drug," in list of known drugs.\nSee data diseasecode for known drugs.")
    atc <- paste0(diseasecode[drug.match][[1]],collapse="|")
    addVariable(x,target=target,type="join") <- selector(data="lmdb",
                                                         var="atc",
                                                         name=drug,
                                                         sortkey="eksd",
                                                         search.term=atc,
                                                         select="first")
    x
}

##' Add a comorbidity to an existing dpp object 
##'
##' Add a comorbidity to an existing dpp object 
##' @title Add comorbidity
##' @param x dpp object
##' @param target name of dataset 
##' @param como character string which should match \code{names(diseasecode)}
##' @param ... not used
##' @return updated object
##' @seealso addSex, addAge, addVariable, addDrug
##' @examples # see help(dpp)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
addComo <- function(x,target,como,...){
    ## extraction of diagnoses
    utils::data(diseasecode)
    como.match <- grep(como,names(diseasecode),ignore.case=TRUE,value=TRUE)
    if (length(como.match)==0) stop("Cannot find definition of comorbidity ",como," in list of known comorbidities.\nSee diseasecode for known comorbidities.")
    icd <- paste0(diseasecode[como.match][[1]],collapse="|")
    addVariable(x,target=target,type="join") <- selector(data="lpr",
                                                         var="diag",
                                                         name=como,
                                                         sortkey="inddto",
                                                         search.term=icd,
                                                         select="first")
    x
}

##' Add sex to an existing dpp object 
##'
##' Add sex to an existing dpp object 
##' @title Add sex
##' @param x dpp object
##' @param target name of dataset 
##' @param ... not used
##' @return updated object
##' @seealso addComo, addAge, addVariable
##' @examples # see help(dpp)
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
addSex <- function(x,target,...){
    addVariable(x,target=target) <- selector(data="pop",
                                             var="sex",
                                             name="sex",
                                             sortkey=NULL,
                                             search.term=NULL,
                                             select="expression",
                                             collect=expression(factor(sex,levels=c(0,1),labels=c("Female","Male"))))
    return(x)
}

##' Add age at index to an existing dpp object 
##'
##' Add at index age to an existing dpp object 
##' @title Add age
##' @param x dpp object
##' @param target name of dataset
##' @param ... not used
##' @return updated object
##' @seealso addComo, addSex, addVariable
##' @examples # see help(dpp)
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
addAge <- function(x,target,...){
    ## birthday
    addVariable(x,target=target) <- selector(data="pop",
                                             var="fdato",
                                             name="birthday",
                                             sortkey=NULL,
                                             search.term=NULL,
                                             select="variable",
                                             collect="fdato")
    ## age at index
    expr <- expression(round(as.numeric(index-fdato)/365.25,1))
    addVariable(x,target=target) <- selector(data="pop",
                                             var="age",
                                             sortkey=NULL,
                                             search.term=NULL,
                                             select="expression",
                                             collect=expr)
    return(x)
}

######################################################################
### dpp.R ends here
