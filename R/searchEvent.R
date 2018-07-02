##' Search for an event in specified time period for each individual.
##'
##' When supplying a data.table in 'periods' this should as minimum include an id variable (with the same name as the id variable in 'dt') and a start date. If an end date is also supplied, the search for an event will be be in the interval from end data to start date (so end date should always be before start date). If no end date is supplied, the periods to search in will be determined by the argument 'interval' by searching in the period from start data - interval to start date; so every person can have a unique start date, but to period to search through has the same length for all persons. ##' 
##' If only a single date is given in 'periods' the periods are again based on 'interval' and will be the same periods for all person: From start date - interval to start date.
##'
##' If an argument to 'atcs' is supplied the event search is carried out for each specified atc code. The output will be a data.table with unique individuals and one column for each atc code, specifying whether or not the given act code was detected in the period for each individual.
##' If no argument is given, the output will simply be a data.table with unique individuals and a column denoting whether or not an event was detected in the given (individual) period. 
##' @title Search for an event 
##' @param dt data.table to search through for event. Should at least contain an id ('pnr') and a date ('date').
##' @param periods Periods in which to search for an event. Can be either a single date or a data.table with individual dates. See details.
##' @param atcs Vector of atc codes. If this is supplied, the detected event will also be requied to have one of the given atc codes (see details).
##' @param interval Number of day to look back from the start dates. See details.
##' @param pnr Variable name to identify the individual. Should be the same in both 'dt' and 'periods' (if this is a data.table). 
##' @param atc Variable name to identify the atc codes in 'dt'.
##' @param date Variable name to identify the date for an event in 'dt'.
##' @param startdate Variable name to identify the start date for the event search in 'periods'.
##' @param enddate Variable name to identify the end date for the event search in 'periods'.
##' @return data.table of unique individuals and whether or not an event happened for each individual. See details.
##' @examples
##'
##' set.seed(333)
##' n <- 200
##' dt <- simPrescriptionData(n, startDate="2005-01-01")
##'
##' ## Look at the data
##' dt
##'
##' ## See how many individuals purchased something within 2006-06-01 and 100 days back.
##' searchEvent(dt=dt,periods=as.Date("2006-06-01"),interval=100, date="eksd")
##'
##' ## Searching with indivual periods for subsample of individuals
##' ## Simulate some dates
##' periods <- data.table::copy(dt[1:101,])
##' periods$pnr <- sample(1:n,101)
##' periods$eksd <- periods$eksd+round(rnorm(101,10,sd=10))
##' periods$enddate <- periods$eksd-sample(1:400,size=101,replace=TRUE)
##'
##' ## Look at the data
##' periods
##'
##' ## Do the search, now with individual dates
##' searchEvent(dt=dt,periods=periods,date="eksd",startdate="eksd")
##'
##' ## Specifying the atcs will look for these particular purchases
##' searchEvent(dt=dt,periods=periods,atcs=c("A07", "A12B"),date="eksd",startdate="eksd")
##'  
##' @author Anders Munch
##' @export
searchEvent <- function(dt,periods,atcs=NULL,interval=NULL,
                        pnr='pnr',atc='atc',date='date',startdate='startdate',enddate='enddate'){
    .SD=NULL
    ## Make internal tmp names
    iN <- list(pnr=quote(InternalTmpPNR),
               date=quote(InternalTmpDATE),
               atc=quote(InternalTmpATC),
               startdate=quote(InternalTmpSTARTDATE),
               enddate=quote(InternalTmpENDDATE))
    ## Check if pnr is present in dt 
    if(!(pnr %in% names(dt))) {
        stop(paste(pnr, "is not present in 'dt'."))
    }
    ## Check if there is a date
    if(!(date %in% names(dt))) {
        stop(paste("No date column is found:",date, "is not present in 'dt'."))
    }        
    ## Check if there is atc if atcs!=NULL
    if(length(atcs)>0){
        if(!(atc %in% names(dt))) {
            stop(paste("No atc column is found:",atc, "is not present in 'dt'.",
                       "\nSpecify a correct name or set 'acts=NULL' to only look at the dates."))
        }   
    }
    ## Make base
    if(length(atcs)>0){
        tmp.base <- dt[,c(pnr,atc,date),with=F]
        setnames(tmp.base, old=c(pnr,atc,date),
                 new=as.vector(sapply(iN[c("pnr","atc","date")],as.character)))
    }else{
        tmp.base <- dt[,c(pnr,date),with=F]
        setnames(tmp.base, old=c(pnr,date),
                 new=as.vector(sapply(iN[c("pnr","date")],as.character)))
    }
    if("data.table" %in% attr(periods, "class")){
        ## Check if pnr is present in periods
        if(!(pnr %in% names(periods))) {
            stop(paste(pnr, "is not present in 'periods'."))
        }
        ## Check if there is a start date
        if(!(startdate %in% names(periods))) {
            stop(paste(startdate, "is not present in 'periods'."))
        }
        ## Check whether to use end date or interval
        if(length(enddate)==0) { # i.e., 'enddate' is set to NULL
            enddate="-*-dummyNONAME-*-"
        }
        if(enddate %in% names(periods)){ # Use the 'enddate'
            if(length(interval)>0) {
                message("The argument 'interval' is ignored. To use it, set 'enddate=NULL'.")
            }
            tmp.periods <- periods[,c(pnr,startdate,enddate),with=F]
            ## Change names to avoid problems with merge
            setnames(tmp.periods, old=c(pnr,startdate,enddate),
                     new=as.vector(sapply(iN[c("pnr","startdate","enddate")],as.character)))
        }else{ # Use the 'interval'
            if(length(interval)==0){
                stop("Cannot determine search period. Specify 'interval' or 'enddate'.")
            }else{
                tmp.periods <- periods[,c(pnr,startdate),with=F]
                setnames(tmp.periods, old=c(pnr,startdate),
                         new=as.vector(sapply(iN[c("pnr","startdate")],as.character)))
                tmp.periods[,as.character(iN$enddate):=eval(iN$startdate)-interval]
            }
        }
        if(length(atcs)>0){
            tmp.text <- "data.table::data.table("
            for (j in atcs){
                tmp.text <- paste0(tmp.text, j,
                                   "=sum(eval(iN$enddate)<eval(iN$date) & eval(iN$date) <eval(iN$startdate) & ",
                                   "eval(iN$atc)==",
                                   "'", j, "'",
                                   ")>0")
                if(j!=atcs[length(atcs)]){
                    tmp.text <- paste0(tmp.text, ", ")
                }
            }
            tmp.text <- paste0(tmp.text, ")")
            tmp.expression <- parse(text=tmp.text)
            tmp.merge <- merge(x=tmp.base, y=tmp.periods, all.x = TRUE, by=as.character(iN$pnr))
            out <- tmp.merge[,eval(tmp.expression),by=eval(as.character(iN$pnr))]
        }else{
            tmp.merge <- merge(x=tmp.base, y=tmp.periods, all.x = TRUE, by=as.character(iN$pnr))
            out <- tmp.merge[,data.table::data.table(event=sum(eval(iN$enddate)<eval(iN$date) & eval(iN$date) <eval(iN$startdate))>0),
                             by=eval(as.character(iN$pnr))] 
        }
    }else{
        if(length(interval)==0){stop("No 'interval' specified.")}
        if(!inherits(periods,"Date")){stop("'periods' should be either a data.table or a single date. \nWhen specifying a single date, use as.Date().")}
        if(length(atcs)>0){
            tmp.text <- "data.table::data.table("
            for (j in atcs){
                tmp.text <- paste0(tmp.text, j,
                                   "=sum(",
                                   as.numeric(periods-interval),
                                   "<eval(iN$date) & eval(iN$date)<",
                                   as.numeric(periods),
                                   "& eval(iN$atc)==",
                                   "'", j, "'",
                                   ")>0")
                if(j!=atcs[length(atcs)]){
                    tmp.text <- paste0(tmp.text, ", ")
                }
            }
            tmp.text <- paste0(tmp.text, ")")
            tmp.expression <- parse(text=tmp.text)
        }else{
            tmp.text <- paste0("data.table::data.table(event=sum(",
                              as.numeric(periods-interval),
                              "<eval(iN$date) & eval(iN$date)<",
                              as.numeric(periods),
                              ")>0)")
            tmp.expression <- parse(text=tmp.text)
        }
        out <- tmp.base[,eval(tmp.expression),
                        by=eval(as.character(iN$pnr))]
    }
    setnames(out, old=as.character(iN$pnr), new=pnr)
    out <- out[,lapply(.SD,function(x){ifelse(is.na(x),"FALSE",x)})] # Is this a good solution???
    return(out)
}
