##' Processing function, to perform calculations according to the data and variables specified in preprocessing object. 
##' 
##' @title Process to estimate prescription lengths and averages
##' @param dpp Data preprocessing object
##' @param treatments If specified, calcations will only be performed
##'     for these treatments.
##' @param id If specified, calculations will only be performed for
##'     these individuals.
##' @param trace If TRUE, messages are outputted for the user.
##' @param collapse Logical. If \code{TRUE}, collapse contiguous
##'     exposure periods with same computed exposure strength to one
##'     and add periods with zero exposure.
##' @param tasks 1, 2 or 3. 1 means calculate only the exposure periods; 2 means calculate only the baseline exposure; and 3 means calculate both. 
##' @param splitting Split the data into 10 chunk to estimate remaining time (and possibly speed up computation). 
##' @author Helene Charlotte Rytgaard
##' @examples
##' set.seed(05021992)
##' N=2
##' da=simPrescriptionData(N)
##' a=simAdmissionData(N)
##' d=dpp()
##' drugdb(d) <- da
##' admdb(d) <- a
##' drug(d, "drug1") <- atc("A07")
##' drug(d, "drug1") <- pack(c(200, 400, 500),
##'                          min = c(100, 200, 250),
##'                          max = c(300, 800, 1000),
##'                          def = c(200, 400, 500))
##' d
##' maxdepot(d) <- 4000
##' pwindow(d) <- 2
##' period(d) <- as.Date(c("1997-01-01", "2012-12-31"))
##' process(d)
##' @export
'process' <- function(dpp,
                      treatments = NULL,
                      id = NULL,
                      trace = FALSE,
                      collapse = TRUE,
                      tasks = 3,
                      splitting = TRUE){
    pdate=pnr=NULL
    # Set the right structure for processed object
    dpp$processed <- structure(list(),
                               class = "dppout")
    dpp$baseline <- list()
    if (is.null(dpp$drugs)) stop("No treatments have been specified in object")
    if (length(treatments) == 0) treatments <- names(dpp$drugs)
    for (drugname in treatments){ 
        ## treatfun <- function(drugname) {
        j            <- (1:length(dpp$drugs))[names(dpp$drugs) == drugname]
        atcs         <- unlist(dpp$drugs[[j]]$atc)
        doses        <- dpp$drugs[[j]]$doses
        maxdepot     <- dpp$drugs[[j]]$maxdepot
        period       <- dpp$drugs[[j]]$period
        N            <- dpp$drugs[[j]]$N
        ## baseline     <- dpp$drugs[[j]]$baseline
        ## interval     <- dpp$drugs[[j]]$interval
        if (is.null(dpp$drugdb)) stop("No drug purchase data in object")
        dpp1   <- dpp$drugdb[atc %in% atcs & pdate <= period[2] & pdate >= period[1], ]
        ## dpp1.baseline <- dpp$drugdb[atc %in% atcs, ]
        ##--- unique id's
        if (length(id) == 0) {
            idunique <- unique(dpp1$id)
            ## idunique.baseline <- unique(dpp1.baseline$id)
        } else {
            idunique <- unique(id[id %in% dpp1$id])
            ## idunique.baseline <- unique(id[id %in% dpp1.baseline$id])
        }
        dosesmissing <- !(dpp1$strength %in% doses$value)
        baddata <- baddata2 <- ""
        if(tasks!=2){ # Check if data is OK for calculating exposure periods
            if (any(dpp1$pdate < 0)) {
                warning("Invalid prescription data: negative dates found")
                baddata <- paste(baddata, "- negative values found in prescription dates\n")
                baddata2 <- paste(baddata2, "- negative values found in prescription dates.\n")
            }
            if (length(N) == 0) N = 2
            if (length(maxdepot) == 0) baddata <- paste(baddata, "- max depot missing\n")
            if (length(period) == 0) baddata <- paste(baddata, "- period missing\n")
            if (length(N) == 0) baddata <- paste(baddata, "- prescription window missing\n")
            if ((length(idunique) == 0) & !(length(period) == 0)) {
                ## warning("No individuals used these drugs in the period:",paste(atcs,collapse=""),paste(period,collapse=" - "))
                baddata <- paste(baddata, paste("- No individuals used:",paste(atcs,collapse="")))
            }
            if (any(dpp1$npack < 0.0001)) {
                ## warning("Invalid prescription data - number of packages < 0.0001")
                baddata <- paste(baddata, "- Invalid prescription data - number of packages < 0.0001\n")
            } 
            if (any(dpp1$ppp < 0.5)) {
                ## warning("Invalid prescription data - pills per package < 0.5")
                baddata <- paste(baddata, "- Invalid prescription data - pills per package < 0.5\n")
            }
            if (any(dosesmissing)) {
                ## warning(paste("Missing doses for ", drugname ,paste(unique(dpp1$strength[dosesmissing]), collapse=", ")))
                baddata <- paste(baddata, paste(paste("- Missing doses for ",drugname, paste(unique(dpp1$strength[dosesmissing]), collapse=", ")),"\n"))
            }
            if (length(doses)<=0) {
                ## warning(paste("No doses specified for ", drugname ,paste(unique(dpp1$strength[dosesmissing]), collapse=", ")))
                baddata <- paste(baddata, paste(paste("- No doses specified for ",drugname, paste(unique(dpp1$strength[dosesmissing]), collapse=", ")),"\n"))
            }
            if (baddata!="") {
                message("Bad data for treatment ", drugname,
                        "\nExposure periods cannot be estimated because of the following:\n",baddata,"\n")
            }
        }
        if(tasks!=1){ # Check if data is OK for calculating baseline exposure
            ## if (length(baseline)<=0) {
            ##     baddata2 <- paste(baddata2, "- No baseline date specified.\n")
            ## }
            ## if (length(interval)<=0) {
            ##     baddata2 <- paste(baddata2, "- No interval specified.\n")
            ## }
            if(length(dpp$comorbperiods)==0){
                baddata2 <- paste(baddata2, "- No comorbidity periods specified.\n")
            }
            else{
                if(attr(dpp$comorbperiods,"status")=="empty"){
                    baddata2 <- paste(baddata2, "- No comorbidity periods specified.\n")
                }
                else{
                    if(attr(dpp$comorbperiods,"status")=="nominal"){
                        message(paste0("Loading data for ", drugname, ".\n"))
                    }
                    .tmpLoad <- internalLoadData(dpp$comorbperiods)
                    dpp$comorbperiods <- .tmpLoad$dppObject
                    if(.tmpLoad$prob.status==T){
                        baddata2 <- paste0(baddata2, .tmpLoad$message)
                    }
                }
            }
            if (baddata2!="") {
                message("Bad data for treatment ", drugname,
                        "\nBaseline exposure cannot be estimated because of the following:\n",baddata2,"\n")
            }
        }
        if((baddata!="" & tasks==1) | (baddata2!="" & tasks==2) | (baddata!="" & baddata2!="")){
            message("Ended without processing anything.\n")
        }else{
            if(baddata2=="" & tasks!=1){
                message(paste0("Data for baseline exposure OK for ",
                               drugname, ", processing..."))
                baseline.out <- searchEvent(dpp1[,data.table::data.table(id,atc,pdate)],
                                            periods=dpp$comorbperiods$data,
                                            pnr="id",
                                            date="pdate",
                                            atcs=dpp$comorbperiods$atcs.codes)
                ## out.comor <- comorbidityDrugs(dpp1.baseline[,data.table::data.table(id,atc,pdate)],
                ##                               code=atcs,
                ##                               date=baseline,
                ##                               interval=interval,
                ##                               pnr='id',
                ##                               eksd='pdate')
                ## out.comor$exposure <- TRUE
                ## if(length(out.comor$id)>0){
                ##     ind <- idunique.baseline %in% out.comor$id
                ## }else{
                ##     ind <- rep(FALSE,length(idunique.baseline))
                ## }
                ## tmp.dt <- data.table(id=idunique.baseline[!ind],exposure=FALSE)
                ## if(FALSE){
                ##     baseline.out <- rbind(out.comor,tmp.dt,fill=TRUE)
                ## }else{
                ##     baseline.out <- rbind(out.comor,tmp.dt,fill=TRUE)
                ##     baseline.out <- baseline.out[,data.table::data.table(id,exposure)]
                ## }
                dpp$baseline[[drugname]] <- baseline.out
                message("done.\n")
            }
            if(baddata=="" & tasks!=2){
                message(paste0("Data for exposure periods OK for ",
                               drugname, ", processing..."))
                if (length(id) == 0) {
                    dpp1 <- dpp1[id %in% idunique]
                    admdat <- dpp$admdb[id %in% idunique]
                }else{
                    admdat <- dpp$admdb
                }
                if (length(admdat)==0) { # Put in empty object
                    # Quick way to make empty admdb object. (Hack to allow that no admdata is given.)
                    a <- simAdmissionData(1)
                    a <- a[pnr==2]
                    tmpd <- dpp()
                    admdb(tmpd) <- a                
                    admdat <- tmpd$admdb
                }
                ## save(testlist3,file="~/tmp/testlist3.rda")
                if(length(idunique)>10000 & splitting==TRUE){
                    ind.split <- cut(as.numeric(factor(idunique)), 10, labels =FALSE)
                    out.list <- vector("list", 10)
                    est.time <- 0
                    for (split in 1:10){
                        split.time <- as.numeric(system.time({
                            split.id <- idunique[ind.split==split]
                            out.list[[split]] <- rbindlist(innerprocess(dat=dpp1[id %in% split.id,], # This might not be necessary?
                                                                        admdat=admdat,
                                                                        doses=doses,
                                                                        idunique=split.id,
                                                                        ## drugname=drugname,
                                                                        N=N,
                                                                        maxdepot=maxdepot,
                                                                        collapse=collapse))
                        })[3])
                        if(split!=10){
                            est.time <- (est.time*(split-1) + split.time)/split
                            out.time <- est.time*(10-split)
                            if(out.time>60){
                                out.time <- paste(floor(out.time/60), "minutes,",
                                                  floor(out.time %% 60), "seconds.")
                            }else{
                                out.time <- paste(floor(out.time), "seconds.")
                            }
                            message(paste0(split, "0% processed, estimated remaining time: ",
                                           out.time))
                        }
                    }
                    out <- rbindlist(out.list)
                }else{
                    out <- rbindlist(innerprocess(dat=dpp1,
                                                  admdat=admdat,
                                                  doses=doses,
                                                  idunique=idunique,
                                                  ## drugname=drugname,
                                                  N=N,
                                                  maxdepot=maxdepot,
                                                  collapse=collapse))
                }
                dpp$processed[[drugname]] <- out
                message("done.\n")
            }
        }
    }
    dpp
}
  
