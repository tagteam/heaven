#' Dataset for nested case-control
#'
#' @title Dataset for nested case-control
#' @param pnr PNR for the individual patients
#' @param time Time of event or censoring
#' @param status Event indicator
#' @param Ncontrols Number of controls per case
#' @param data Dateset containing the variables (not the matching and inclusion variables yet)
#' @param match List of matching variables
#' @param include List of covariates to include in the output dataset
#' @return A data.table with the variables pnr, time, status and chosen covariates
#' @details The time variable can be used as strata variable in clogit
#' @examples
#' n <- 100000
#' pnr <- 1:n
#' time <- rexp(n)
#' status <- sample(0:1,n,replace=TRUE,prob=c(.99,.01))
#' sex <- sample(c("M","F"),n,replace=TRUE)
#' age <- sample(c("y","m","o"),n,replace=TRUE)
#' d <- nccSampling(pnr,time,status,Ncontrols=5,match=list(sex,age),include=list(sex=sex,age=age))
#' @export
#' @author Jeppe E. H. Madsen <vcl891@alumni.ku.dk>
nccSampling <- function(pnr,time,status,Ncontrols=10,data=NULL,match=NULL,include=NULL){
    tmp <- NULL
    pnr <- eval(substitute(pnr),data)
    time <- eval(substitute(time),data)
    status <- eval(substitute(status),data)
    ## Matching part of code
    grp <- rep(1,length(time))
    m <- length(match)
    pd <- 1
    if(m > 0){
        for(im in 1:m){
            v <- match[[im]]
            if(!is.factor(v)) v <- factor(v)
            grp <- grp  + pd * (as.numeric(v)-1)
            pd <- pd * length(levels(v))
        }
    }
    for(i in 1:length(unique(grp))){
        tmppnr <- pnr[grp==i]
        tmptime <- time[grp==i]
        tmpstatus <- status[grp==i]
        tmp <- rbind(tmp, nccSamplingCpp(tmppnr,tmptime,tmpstatus,Ncontrols))
    }
    ## Include part of code
    mm <- length(include)
    if(mm > 0){
        cov <- matrix(NA,nrow=nrow(tmp),ncol=mm)
        for(i in 1:mm){
            v <- include[[i]]
            cov[,i] <- v[match(tmp$pnr,pnr)]
        }
        colnames(cov) <- names(include)
        tmp <- cbind(tmp, cov)
    }
    ## Return as sorted data table
    tmp <- setDT(tmp)
    tmp <- tmp[time!=0,]
    tmp[order(time),]
}
