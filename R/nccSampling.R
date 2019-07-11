#' Dataset for nested case-control
#'
#' @title Create dataset for nested case-control design 
#' @param pnr PNR for the individual patients
#' @param time Time of event or censoring
#' @param status Event indicator
#' @param Ncontrols Number of controls per case
#' @param data Dateset containing the variables (not the matching and inclusion variables yet)
#' @param match List of matching variables
#' @param include List of covariates to include in the output dataset
#' @param Tstart Time of treatment start
#' @param exposureWindow Minimum required time of exposure to be included both as a case and as a control
#' @param cores Number of cores to use
#' @return A data.table with the variables pnr, strata, case and chosen covariates
#' @examples
#' \dontrun{n <- 100000
#' pnr <- 1:n
#' time <- rexp(n)
#' status <- sample(0:1,n,replace=TRUE,prob=c(.99,.01))
#' sex <- sample(c("M","F"),n,replace=TRUE)
#' age <- sample(c("y","m","o"),n,replace=TRUE)
#' data <- data.frame(pnr,time,status,sex,age)
#' d <- nccSampling(pnr,time,status,Ncontrols=5,match=list(sex,age),data=data,Ncontrols=5)
#' }
#' @export
#' @useDynLib heaven
#' @author Jeppe E. H. Madsen <vcl891@alumni.ku.dk>
nccSampling <- function(pnr,time,status,Ncontrols=10L,data=NULL,match=NULL,include=NULL,
                        Tstart=rep(0,length(pnr)),exposureWindow=0,cores=1){
    tmp <- NULL
    match <- eval(substitute(match),data)
    if(is.null(include)) include <- match
    include <- eval(substitute(include),data)
    pnr <- eval(substitute(pnr),data)
    time <- eval(substitute(time),data)
    status <- eval(substitute(status),data)
    Tstart <- eval(substitute(Tstart),data)
    sub <- ((time-Tstart)>exposureWindow)
    pnr <- pnr[sub]
    time <- time[sub]
    status <- status[sub]
    Tstart <- Tstart[sub]
    m <- length(match)
    for(i in 1:m) match[[i]] <- match[[i]][sub];
    ii <- length(include)
    for(i in 1:ii) include[[i]] <- include[[i]][sub]
    exposureWindow <- rep(exposureWindow, length(pnr))
    if(!is.numeric(pnr)){
        pnr <- as.numeric(as.factor(pnr))
        warning("pnr not numeric - output pnr might be different from input pnr")
    } 
    ## Matching part of code
    ifelse(is.null(match), grp <- rep(1,length(time)), grp <- as.numeric(interaction(match)))
    if(cores==1){
        for(i in 1:length(unique(grp))){
            ind <- (grp==i)
            tmppnr <- pnr[ind]
            tmptime <- time[ind]
            tmpstatus <- status[ind]
            tmpTstart <- Tstart[ind]
            tmpexposureWindow <- exposureWindow[ind]
            tmp <- rbind(tmp, nccSamplingCpp(tmppnr,tmptime,tmpstatus,tmpTstart,
                                             tmpexposureWindow,Ncontrols))
        }
    }
    else{
        registerDoMC(cores)
        tmp <- foreach(i=1:length(unique(grp)), .combine = "rbind") %dopar% {
            ind <- (grp==i)
            tmppnr <- pnr[ind]
            tmptime <- time[ind]
            tmpstatus <- status[ind]
            tmpTstart <- Tstart[ind]
            tmpexposureWindow <- exposureWindow[ind]
            nccSamplingCpp(tmppnr,tmptime,tmpstatus,tmpTstart,tmpexposureWindow,Ncontrols)
        }
    }
    ## Include part of code
    if(ii > 0){
        cov <- as.data.frame(matrix(NA,nrow=nrow(tmp),ncol=ii))
        for(i in 1:ii){
            v <- include[[i]]
            cov[,i] <- v[match(tmp$pnr,pnr)]
        }
        colnames(cov) <- names(include)
        tmp <- cbind(tmp, cov)
    }
    ## Return as sorted data table
    setDT(tmp)
    tmp <- tmp[pnr!=0,]
    if(sum(tmp$status==0) != (sum(tmp$status==1)*Ncontrols))
        warning(paste0("Some stratas incomplete due to too few observations at risk"))
    colnames(tmp)[1:3] <- c("pnr","strata", "case")
    tmp$strata <- as.numeric(as.factor(match(tmp$strata, tmp$strata)))
    tmp[order(strata),]
}
