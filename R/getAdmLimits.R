### getAdmLimits.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Aug  4 2016 (19:43)
## Version:
## last-updated: Jul 10 2019 (08:50) 
##           By: Thomas Alexander Gerds
##     Update #: 74
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Adds first admission date and last discharge date to a sequence of admissions. The default input data.table is the current standard
##' by which admissions are available in Statistics Denmark. 
##'
##' @title Get Admission time
##' @param dt data.table containing the variables
##' @param pnr Variable with ID for each subject/group (default is \code{pnr}).
##' @param inddto Variable with entry times. Must be date or numeric (default is \code{inddto}). 
##' @param uddto Variable with exit times. Must be date or numeric (default is \code{uddto}).
##' @param collapse if \code{TRUE} return only the lines with non-overlapping admissions
##' @param error Character. One of \code{"warn"}, \code{"remove"} remove lines with errors,
##' \code{"flag"} add a new variable called error. An error is defined as either a missing value in
##' either inddto or uddto or when inddto>uddto.
##' @return data.table with two new variables first.indate and last.outdate.
##' @examples
##' set.seed(8)
##' lpr <- simAdmissionData(10)
##' ## Variables have default names
##' adm <- getAdmLimits(lpr)
##'
##' ## with errors
##' lpr1 <- lpr
##' lpr1 <- lpr1[uddto>as.Date("2010-01-01"),uddto:=NA]
##' adm1 <- getAdmLimits(lpr1)
##' adm1 <- getAdmLimits(lpr1,error="remove")
##' adm1 <- getAdmLimits(lpr1,error="flag")
##' 
##' ## case where Variables have custom names
##' data.table::setnames(lpr, c('pnr','inddto','uddto'), c('personid', 'Entrydate','Outdate'))
##' adm2 <- getAdmLimits(lpr,pnr='personid',inddto='Entrydate',uddto='Outdate')
##'
##' ## duplicated and overlap
##' set.seed(8)
##' lpr3 <- simAdmissionData(10)
##' lpr3 <- lpr3[sample(1:NROW(lpr3),replace=TRUE,size=round(NROW(lpr3)*1.5))]
##' adm3 <- getAdmLimits(lpr3,collapse=TRUE)
##' 
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
getAdmLimits <- function(dt,
                         pnr="pnr",
                         inddto="inddto",
                         uddto="uddto",
                         collapse=FALSE,
                         error="warn"){
    first.indate <- .N <- last.outdate <- NULL
    ## take a hard copy
    wdt=data.table::copy(dt)
    if (!is.data.table(wdt)){
        setDT(wdt)
    }
    orig.names <- copy(names(wdt))
    ## Check variable types
    datecheckerInd=FALSE
    if (class(wdt[[inddto]])=="Date"){
        datecheckerInd=TRUE
    } else if (!is.numeric(wdt[[inddto]])){
        stop("Error: inddto must be numeric or Date")
    }  
    datecheckerUd=FALSE
    if (class(wdt[[uddto]])=="Date"){
        datecheckerUd=TRUE
    } else if (!is.numeric(wdt[[uddto]])){
        stop("Error: uddto must be numeric or Date")
    }    
    ## flag rows with inddto>uddto and NA in inddto
    wdt[,error:=is.na(.SD[[1]])|is.na(.SD[[2]])|.SD[[1]]>.SD[[2]],.SDcols=c(inddto,uddto)]
    switch(error,"flag"={
        ## done this already
    },"remove"={
        wdt <- wdt[wdt[["error"]]==FALSE]
        wdt[,error:=NULL]
    },"warn"={
        if (any(wdt[["error"]])){
            warning("Data have errors: ",immediate.=TRUE)
            print(wdt[wdt[["error"]]])
        }else{
            wdt[,error:=NULL]
            message("No data errors.")
        }
    })
    ## Convert Dates to numeric
    if (datecheckerInd==TRUE){
        set(wdt,j=inddto,value=as.integer(wdt[[inddto]]))
    }
    if (datecheckerUd==TRUE){
        set(wdt,j=uddto,value=as.integer(wdt[[uddto]]))
    }
    ## Sort by pnr and admission dates
    setkeyv(wdt,c(pnr,inddto))
    ## Latest admission date by pnr
    wdt[,`:=`(last.outdate=cummax(.SD[[1]])),.SDcols=uddto,by=pnr] 
    ## Start of each admission 
    wdt[,first.indate:= cumsum(c(1L,(1L*(.SD[[1]]>shift(.SD[[2]],n=1)))[-1])),.SDcols=c(inddto,"last.outdate"),by=pnr]
    ## Start and end date for the hospitalization
    wdt[,':='(first.indate=.SD[[1]][1] , last.outdate=.SD[[2]][.N]),.SDcols=c(inddto,"last.outdate"),by=c(pnr,"first.indate")]
    ## Change columns around
    if ("error"%in%names(wdt))
        setcolorder(wdt, c(orig.names,"first.indate","last.outdate","error"))
    else
        setcolorder(wdt, c(orig.names,"first.indate","last.outdate"))
    ## Convert back in case input was format Date
    if(datecheckerInd==TRUE){
        set(wdt,j=inddto,value=as.Date(wdt[[inddto]],origin="1970-01-01"))
    }
    if(datecheckerUd==TRUE){
        set(wdt,j=uddto,value=as.Date(wdt[[uddto]],origin="1970-01-01"))
    }
    wdt[,first.indate:=as.Date(first.indate,origin="1970-01-01")]
    wdt[,last.outdate:=as.Date(last.outdate,origin="1970-01-01")]
    if (collapse){
        ## wdt <- wdt[,.SD[1],by=c(pnr,"first.indate","last.outdate")]
        n.before <- NROW(wdt)
        collapse.cols <- c(pnr,"first.indate","last.outdate")
        wdt <- wdt[wdt[, .I[1],by=collapse.cols]$V1]
        message("Collapsed ",n.before," rows with multiple and overlapping periods into ",NROW(wdt)," rows.")
    }
    ## Create list and return
    class(wdt) <- c("admlimits",class(wdt))
    wdt[]
}

#----------------------------------------------------------------------
### getAdmLimits.R ends here
