##' Processing function, to perform calculations according to the data and variables specified in preprocessing object. 
##' 
##' @title Process to estimate prescription lengths and averages 
##' @param dpp Data preprocessing object
##' @param treatments If specified, calcations will only be performed for these treatments. 
##' @param id If specified, calculations will only be performed for these individuals.
##' @param trace If TRUE, messages are outputted for the user. 
##' @param collapse Logical. If \code{TRUE}, collapse contiguous exposure periods
##'        with same computed exposure strength to one and add periods with zero exposure.
##' @param keep_data If TRUE, the input data is saved in the object as well. This can can be used for plotting. 
##' @author Helene Charlotte Rytgaard
##' @examples
##' set.seed(05021992)
##' N=2
##' da=heaven:::simPrescriptionData(N)
##' a=heaven:::simAdmissionData(N)
##' library(heaven)
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
##' process(d,collapse=TRUE) <- "drug1"
##' @export
'process<-' <- function(dpp,
                        treatments = NULL,
                        id = NULL,
                        trace = FALSE,
                        collapse = TRUE,value){
    
    if (length(treatments) == 0) treatments <- names(dpp$drugs)
    for (treatname in treatments){ 
        ## treatfun <- function(treatname) {
        j            <- (1:length(dpp$drugs))[names(dpp$drugs) == treatname]
        atcs         <- unlist(dpp$drugs[[j]]$atc)
        doses        <- dpp$drugs[[j]]$doses
        maxdepot     <- dpp$drugs[[j]]$maxdepot
        period       <- dpp$drugs[[j]]$period
        N            <- dpp$drugs[[j]]$N
        dpp1   <- dpp$drugdb[atc %in% atcs & pdate <= period[2] & pdate >= period[1], ]
        ##--- unique id's
        if (length(id) == 0) {
            idunique <- unique(dpp1$id)
        } else 
            idunique <- unique(id[id %in% dpp1$id])
        dosesmissing <- !(dpp1$strength %in% doses$value)
        baddata <- ""
        if (length(N) == 0) N = 2
        if (length(maxdepot) == 0) baddata <- paste(baddata, " : max depot missing\n")
        if (length(period) == 0) baddata <- paste(baddata, " : period missing\n")
        if (length(N) == 0) baddata <- paste(baddata, " : prescription window missing\n")
        if (any(dpp1$pdate < 0)) {
            warning("Invalid prescription data: negative dates found")
            baddata <- paste(baddata, " : negative values found in prescription dates\n")
        } 
        if (length(idunique) == 0) {
            ## warning("No individuals used these drugs in the period:",paste(atcs,collapse=""),paste(period,collapse=" - "))
            baddata <- paste(baddata, paste("No individuals used ",paste(atcs,collapse="")))
        }
        if (any(dpp1$npack < 0.0001)) {
            ## warning("Invalid prescription data - number of packages < 0.0001")
            baddata <- paste(baddata, "Invalid prescription data - number of packages < 0.0001\n")
        } 
        if (any(dpp1$ppp < 0.5)) {
            ## warning("Invalid prescription data - pills per package < 0.5")
            baddata <- paste(baddata, "Invalid prescription data - pills per package < 0.5\n")
        }
        if (any(dosesmissing)) {
            ## warning(paste("Missing doses for ", treatname ,paste(unique(dpp1$strength[dosesmissing]), collapse=", ")))
            baddata <- paste(baddata, paste(paste(" : Missing doses for ",treatname, paste(unique(dpp1$strength[dosesmissing]), collapse=", ")),"\n"))
        }
        if (length(doses)<=0) {
            ## warning(paste("No doses specified for ", treatname ,paste(unique(dpp1$strength[dosesmissing]), collapse=", ")))
            baddata <- paste(baddata, paste(paste(" : No doses specified for ",treatname, paste(unique(dpp1$strength[dosesmissing]), collapse=", ")),"\n"))
        }
        if (baddata!="") {
            cat("Bad data for treatment ", treatname, "(will terminate):\n",baddata,"\n")
        } else {
            if (length(id) == 0) {
                dpp1 <- dpp1[id %in% idunique]
                admdat <- dpp$admdb[id %in% idunique]
            }else{
                admdat <- dpp$admdb
            }
            ## testlist3 <- list(dat=dpp1,admdat=admdat,doses=doses,idunique=idunique,treatname=treatname,N=N,maxdepot=maxdepot,collapse=collapse)
            ## save(testlist3,file="~/tmp/testlist3.rda")
            out <- rbindlist(innerprocess(dat=dpp1,
                                          admdat=admdat,
                                          doses=doses,
                                          idunique=idunique,
                                          treatname=treatname,
                                          N=N,
                                          maxdepot=maxdepot,
                                          collapse=collapse))
            dpp$processed[[treatname]] <- out
        }
    }
    dpp
}
  
