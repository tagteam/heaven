##' Processing function, to perform calculations according to the data and variables specified in preprocessing object. 
##' 
##' @title Medicin macro to estimate prescription lengths and averages
##' @param drugs List of drugs (see examples).
##' @param treatments If specified, calcations will only be performed
##'     for these treatments.
##' @param drugdb data.table with (subset of) medical drugs registry
##' @param admdb data.table with (subset of) hospital admission registry
##' @param drugdb.datevar name of the date variable in \code{drugdb}. Default is \code{"eksd"}.
##' @param admdb.datevars names of the date variables in \code{admdb}. Default is \code{c("inddto","uddto")}.
##' @param id Name of variable to identify individuals. Default is
##'     \code{"pnr"}.
##' @param strength.var Name of variable to identify strength. Default
##'     is \code{"strnum"}.
##' @param packsize.var Name of variable to identify size of
##'     packages. Default is \code{"packsize"}.
##' @param apk.var Name of variable to identify number of
##'     packages. Default is \code{"packsize"}.
##' @param collapse Logical. If \code{TRUE}, collapse contiguous
##'     exposure periods with same computed exposure strength to one
##'     and add periods with zero exposure.
##' @param splitting Split the data into 10 chunks to estimate
##'     remaining time (and possibly speed up computation).
##' @author Helene Charlotte Rytgaard, Anders Munch and Thomas Alexander Gerds based
##'     on the famous SAS macro by Christian Tobias Torp-Pedersen
##' @examples
##' set.seed(05021992)
##' N=20
##' lmdb=simPrescriptionData(N)
##' lpr=simAdmissionData(N)
##' drug1 = list(atc=c("R03AK11","R03AL03","R03AC02","R03AC04","R03AC19",
##'                     "R03AL02","R03AA01","R03AC18","R03AL01"),
##'              maxdepot=4000,
##'              period=as.Date(c("1997-01-01", "2012-12-31")),
##'              prescriptionwindow=2,
##'              doses=list(value=c(750,500,400,200,75),
##'                         min = c(250,200,200,100,25),
##'                         max = c(1000,600,800,600,100),
##'                         def = c(750,500,400,200,75)))
##' drug2=list(atc=c("C01CA01","C01AA05"),
##'            maxdepot=4000,
##'            period=as.Date(c("1997-01-01", "2012-12-31")),
##'            prescriptionwindow=2,
##'            doses=list(value=c(200, 400, 500,750),
##'                       min = c(100, 200, 250,750),
##'                       max = c(300, 800, 1000,750),
##'                       def = c(200, 400, 500,750)))
##' a <- medicinMacro(drugs=list("drug1"=drug1,"drug2"=drug2),drugdb=lmdb,admdb=lpr)
##' @export
medicinMacro <- function(drugs,
                         treatments = NULL,
                         drugdb,
                         admdb,
                         drugdb.datevar="eksd",
                         admdb.datevars=c("inddto","uddto"),
                         id = "pnr",
                         strength.var = "strnum",
                         packsize.var="packsize",
                         apk.var="apk",
                         collapse = TRUE,
                         splitting = FALSE){
    atc=eksd=inddto=uddto=pnr=NULL
    # Set the right structure for processed object
    processed <- structure(list(),class = "medicinmacro")
    if (missing(drugs) || is.null(drugs)) stop("Sorry, no treatments have been specified.")
    if (length(treatments) == 0) treatments <- names(drugs)
    if (missing(drugdb) || is.null(drugdb)) stop("No drug purchase data provided")
    for (drugname in treatments){ 
        ## treatfun <- function(drugname) {
        j            <- (1:length(drugs))[names(drugs) == drugname]
        atcs         <- unlist(drugs[[j]]$atc)
        doses        <- drugs[[j]]$doses
        maxdepot     <- drugs[[j]]$maxdepot
        period       <- drugs[[j]]$period
        prescriptionwindow            <- drugs[[j]]$prescriptionwindow
        drugdb.work <-  copy(drugdb)
        if (id!="pnr") setnames(drugdb.work,id,"pnr")
        if (strength.var!="strnum") setnames(drugdb.work,strength.var,"strnum")
        if (packsize.var!="packsize") setnames(drugdb.work,packsize.var,"packsize")
        if (apk.var!="apk") setnames(drugdb.work,apk.var,"apk")
        if (drugdb.datevar!="eksd") {
            setnames(drugdb.work,drugdb.datevar,"eksd")
        }
        admdb.work <-  copy(admdb)
        if (any(admdb.datevars!=c("inddto","uddto"))) {
            setnames(admdb.work,admdb.datevars[1],"inddto")
            setnames(admdb.work,admdb.datevars[2],"uddto")
        }    
        drugdb.work   <- drugdb.work[atc %in% atcs & eksd <= period[2] & eksd >= period[1], ]
        admdb.work   <- admdb.work[inddto<= period[2] & uddto >= period[1], ]
        ##--- unique id's
        idunique <- unique(drugdb.work[["pnr"]])
        if (length(idunique)==0) {
            warning(paste0("No individual purchased ",paste0(atcs,collapse=", ")," in this period"))
            processed[[drugname]] <- NULL
        }else{
            # Check if data is OK for calculating exposure periods
            if (length(prescriptionwindow) == 0) prescriptionwindow = 2
            if (length(maxdepot) == 0) stop("Argument max depot missing\n")
            if (length(period) == 0) stop("Argument period missing\n")
            if (length(doses)<=0) {
                stop(paste("- No doses specified for ",drugname, paste(unique(drugdb.work[[strength.var]][dosesmissing]), collapse=", ")),"\n")
            }
            dosesmissing <- !(unique(drugdb.work[[strength.var]]) %in% doses$value)
            if (any(dosesmissing)) {
                stop(paste0(paste0("Missing doses (min,max,def) for ",drugname,": ",
                                   paste(unique(drugdb.work[[strength.var]])[dosesmissing],collapse=", ")),"\n"))
            }
            if(length(idunique)>10000 & splitting==TRUE){
                ind.split <- cut(as.numeric(factor(idunique)), 10, labels =FALSE)
                out.list <- vector("list", 10)
                est.time <- 0
                for (split in 1:10){
                    split.id <- idunique[ind.split==split]
                    setkeyv(drugdb.work,pnr)
                    out.list[[split]] <- rbindlist(innerprocess(dat=drugdb.work[split.id], 
                                                                admdat=admdb.work,
                                                                doses=doses,
                                                                idunique=split.id,
                                                                prescriptionwindow=prescriptionwindow,
                                                                maxdepot=maxdepot,
                                                                collapse=collapse))
                }
                out <- rbindlist(out.list)
            }else{
                out <- rbindlist(innerMedicinMacro(dat=drugdb.work,
                                                   admdat=admdb.work,
                                                   doses=doses,
                                                   idunique=as.numeric(idunique),
                                                   prescriptionwindow=prescriptionwindow,
                                                   maxdepot=maxdepot,
                                                   collapse=collapse))
            }
            processed[[drugname]] <- out
        }
    }
    processed
}
  
