##' Compute standardized absolute risks based on a stratified Kaplan-Meier or Aalen-Johansen estimate
##'
##' Fit prodlim with two categorical covariates, say age and sex. Then
##' standardize the absolute risk within each of the two gender to the
##' age distribution in whole data set.
##' @title Standardize absolute risks
##' @param object Object obtained with \code{prodlim} stratified with
##'     two categorical covariates.
##' @param var1 Name of one of the two categorical variables. The
##'     absolute risks are standardized within the levels of this
##'     variables to the distribution of the other categorical
##'     variable in the full data.
##' @param var2 Name of the other of the two categorical variables,
##'     i.e., the one on which the weights are based.
##' @param times Compute absolute risks at these time points
##' @param data Only if weights are not given. Data set in which to compute the weights for
##'     standardization as percentage in each of the levels of \code{var2}.
##' @param weights A numeric vector of weights that sum to 1 and has length equal
##' to the number of strata defined by \code{var2}. If omitted, weights are calculated using \code{data}.
##' @param ... not used
##' @examples
##' library(survival)
##' library(prodlim)
##' library(data.table)
##' data(pbc)
##' d <- na.omit(pbc[,c("time","status","trt","age","sex")])
##' d$agegroup <- cut(d$age,breaks=c(0,40,50,60,100))
##' fit <- prodlim(Hist(time,status==1)~trt+agegroup,data=d)
##' standardize.prodlim(fit,var1="trt",var2="agegroup",data=d,times=1000)
##' standardize.prodlim(fit,var1="trt",var2="agegroup",data=d,times=c(1000,1500))
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
standardize.prodlim <- function(object,
                                var1,
                                var2,
                                times,
                                data,
                                weights,
                                ...){
    X <- data.table(object$X)
    data.table::setkeyv(X,c(var1,var2))
    stopifnot(NCOL(X)==2)
    if (missing(weights)){
        weights <- prop.table(table(data[[var2]]))
    }else{
        stopifnot(length(weights)==length(unique(X[[var2]])))
    }
    worktable <- predict(object,newdata=X,time=times,level.chaos=1,mode="matrix",type="cuminc")
    colnames(worktable) <- paste0("risk",times)
    rrr <- strsplit(rownames(worktable),", ")
    worktable <- data.frame(worktable,row.names=NULL)
    data.table::setDT(worktable)
    v1 <- sapply(rrr,function(r){sapply(strsplit(r[[1]],"="),function(u)u[[2]])})
    v2 <- sapply(rrr,function(r){sapply(strsplit(r[[2]],"="),function(u)u[[2]])})
    worktable <- cbind(v1,v2,worktable)
    colnames(worktable)[1:2] <- c(var1,var2)
    out <- worktable[,lapply(.SD,function(x)x*as.vector(weights)),by=var1,.SDcols=paste0("risk",times)]
    out
}


