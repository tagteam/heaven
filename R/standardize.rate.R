##' Standardize proportions and absolute risks to a given age distribution
##'
##' Standardize proportions and absolute risks to a given age
##' distribution
##' @title Standardize proportions and absolute risks to a given age
##'     distribution
##' @param x List of names of variable names used to calculate the rates.
##'          Each element of the list contains the names of two variables in the
##'          dataset: the first variable contains the number of events and the second
##'          variable contains the number of subjects or person years.
##' @param age Name of categorical age variable. 
##' @param exposure Name of the exposure variable for rate ratios.
##' @param by Vector of names of further categorical strata variables
##' @param standardize.to what population to use for standardization.
##' @param data Data set which contains all the variables
##' @param method Character. The method for calculating confidence intervals.
##'        If "gamma" use gamma distribution (see Fay et al.). If "wald" or "wald-log" use
##'        normal or log-normal approximation. 
##' @param level Confidence level
##' @param crude Logical. If \code{TRUE} calculate crude rates too.
##' @param ... Not (yet) used
##' @references
##' Michael P Fay. Approximate confidence intervals for rate ratios from
##' directly standardized rates with sparse data. Communications in Statistics-
##' Theory and Methods, 28(9):2141--2160, 1999.
##'
##' Niels Keiding, David Clayton, et al. Standardization and control for
##' confounding in observational studies: a historical perspective. Statistical
##' Science, 29(4):529--558, 2014.
##' @return Data table with standardized rates (and crude rates if asked for) 
##' @seealso standardize.prodlim standardize.proportion epitools::ageadjust.direct
##' @examples
##' library(riskRegression)
##' library(data.table)
##' set.seed(84)
##' n=160
##' d <- data.table(e1=rpois(n,lambda=9),
##'                 rt1=rpois(n,lambda=1880),
##'                 e2=rpois(n,lambda=123),
##'                 rt2=rpois(n,lambda=80000))
##' d[,agegroups:=factor(rep(c("40-50","45-50","50-55","55-60","60-65","65-70","70-75","75-80"),n/8))]
##' d[,sex:=factor(rep(c("f","m"),c(n/2,n/2)))]
##' d[,year:=rep(2001:2010,n/10)]
##' D=d[,.(e1=sum(e1),rt1=sum(rt1),e2=sum(e2),rt2=sum(rt2)),by=c("sex","agegroups")]
##' D[sex=="m",e1:=e1+rpois(.N,lambda=as.numeric(agegroups)*17)]
##' D[sex=="m",rt1:=rt1-rpois(.N,lambda=as.numeric(agegroups)*1600)]
##' standardize.rate(x=list(c("e1","rt1")),
##'                  age="agegroups",exposure="sex",data=D,standardize.to="f")
##' standardize.rate(x=list(c("e1","rt1")),
##'                  age="agegroups",exposure="sex",data=D,standardize.to="m")
##' standardize.rate(x=list(c("e1","rt1")),
##'                  age="agegroups",exposure="sex",data=D,standardize.to="mean")
##' standardize.rate(x=list("rate1"=c("e1","rt1"),"rate2"=c("e2","rt2")),
##'                  age="agegroups",exposure="sex",data=d,by="year")
##'
##' # more than 2 exposures does not yet work!! workaround is to subset ...
##' \dontrun{
##' d[,groups:=factor(rep(paste0("G",1:4),rep(n/4,4)))]
##' D=d[,.(e1=sum(e1),rt1=sum(rt1),e2=sum(e2),rt2=sum(rt2)),by=c("groups","agegroups")]
##' D[groups=="G3",e1:=e1+rpois(.N,lambda=as.numeric(agegroups)*17)]
##' D[groups=="G3",rt1:=rt1-rpois(.N,lambda=as.numeric(agegroups)*1600)]
##' standardize.rate(x=list(c("e1","rt1")),
##'                  age="agegroups",exposure="groups",data=D,standardize.to="mean")
##' }
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
standardize.rate <- function(x,
                             age="agegroups",
                             exposure,
                             by,
                             standardize.to="ref.level",
                             data,
                             method="gamma",
                             level=0.95,
                             crude=TRUE,
                             ...){
    requireNamespace("data.table")
    .N=N=.SD=weight=NULL
    setDT(data)
    data <- copy(data)
    nnn <- colnames(data)
    if (!is.list(x) || any(sapply(x,length)!=2)){
        stop("Argument 'x' has to be a list where each element contains two variable names:\n1. number of events\n2. number at risk (or person-years)")
    }
    if (!is.character(age) || match(age,nnn,nomatch=0)==0) stop("Argument 'age' has to be the name of the age group variable in the dataset.")
    if (!is.character(exposure) || match(exposure,nnn,nomatch=0)==0) stop("Argument 'exposure' has to be the name of the exposure group variable in the dataset.")
    if (!missing(by)){
        if (length(by)>=NROW(data)) stop("Length of 'by' argument exceeds length of data.")
        if (!all(sapply(by,is.character)) || any(match(by,nnn,nomatch=0)==0)) stop("Argument 'by' has to be a vector of variable names in the dataset.")
    }
    if (!is.factor(data[[age]])){
        stop("Age variable has to be grouped as factor in the dataset.")
    }
    if (!is.factor(data[[exposure]])){
        data[[exposure]] <- factor(data[[exposure]])
    }
    exposure.levels <- levels(data[[exposure]])
    if (missing(standardize.to)) {                                                  
        standardize.to <- "ref.level"
    } else{
        if (!is.character(standardize.to)) stop("Argument 'standardize.to' is not character. It has characterize the standard population.")        
    }
    N <- NROW(data)
    n0 <- length(exposure.levels)
    out <- data[,{
        xout <- rbindlist(lapply(1:length(x),function(v){
            counts <- .SD[[x[[v]][[1]]]]
            pops <- .SD[[x[[v]][[2]]]]  
            egroups <- .SD[[exposure]]
            ## Jeppe laver rod
            e1 <- egroups==exposure.levels[[1]]
            std.x <- NULL
            for(i in 2:n0){
                e2 <- egroups==exposure.levels[i]
                stdpop <- switch(standardize.to, ref.level = {
                    pops[e1]
                }, mean = {
                    (pops[e1] + pops[e2])/2
                }, pops[e2])
                std.x <- rbind(std.x, dsr(count0=counts[e1], count1=counts[e2],
                                          pop0=pops[e1], pop1=pops[e2], stdpop=stdpop,
                                          method=method,crude=crude))
            }
            xname <- ifelse(length(names(x)[v])==0,x[[v]][1],names(x)[v])
            std.x <- cbind(x=xname,std.x)
        }))
        xout
    },.SDcols=c(unlist(x),exposure),by = by]
    out
}

##' Function to Compute confidence interval for directly standardized rates and rate ratios
##'
##' Function to Compute confidence interval for directly standardized rates
##' and rate ratios for sparse data. Method implemented include gamma confidence
##' intervals (for DSR), exact confidence intervals (for crude rates),
##' the inverse of the F distribution (for DSR ratio)
##' and some Wald confidence interval (also on log-scale) for comparison purpose.
##' @title Confidence intervals for age standardized rates and rate ratios
##' @param count1 counts for group 1 (e.g. exposed)
##' @param pop1 number of subjects of person-years in group 1 
##' @param count0 counts for group 1 (e.g. exposed)
##' @param pop0 number of subjects of person-years in group 0 
##' @param stdpop number of subjects of person-years in stdpop population 
##' @param conf.level confidence level of confidence intervals
##' @param method method for calculating confidence intervals
##' @param crude logical. if \code{TRUE} also calculate crude rates 
##' @references
##' Fay, Michael P., and Eric J. Feuer. "Confidence intervals for directly
##' standardized rates: a method based on the gamma distribution." Statistics
##' in Medicine 16.7 (1997): 791-801.
##'
##' Fay, Michael P. "Approximate confidence intervals for rate ratios from
##' directly standardized rates with sparse data." Communications in
##' Statistics-Theory and Methods 28.9 (1999): 2141-2160.
##'
##' Fay, Michael P., et al. "Estimating average annual percent change
##' for disease rates without assuming constant change." Biometrics
##' 62.3 (2006): 847-854.
##'
##' Fay, Michael P., Michael A. Proschan, and Erica Brittain. Combining
##' one-sample confidence procedures for inference in the two-sample case.
##' Biometrics 71.1 (2015): 146-156.
##'
##' Interesting other approaches that avaoid inconsistency between
##' exact CI and p-values are implemented in exactci and exact2x2, see
##'
##' Fay, Michael P. "Two-sided exact tests and matching confidence intervals
##' for discrete data." R journal 2.1 (2010): 53-58.
##' 
##' @return 
##' List with crude and standardized rates and rate ratios. 
##'
##' @seealso epitools::ageadjust.direct
##' @examples
##' library(riskRegression)
##' library(data.table)
##' set.seed(84)
##'  n=160
##' d <- data.table(e1=rpois(n,lambda=9),
##'                 rt1=rpois(n,lambda=1880),
##'                 e2=rpois(n,lambda=123),
##'                 rt2=rpois(n,lambda=80000))
##' d[,agegroups:=factor(rep(c("40-50","45-50","50-55","55-60","60-65","65-70","70-75","75-80"),n/8))]
##' d[,sex:=factor(rep(c("f","m"),c(n/2,n/2)))]
##' d[,year:=rep(2001:2010,n/10)]
##' D=d[,.(e1=sum(e1),rt1=sum(rt1),e2=sum(e2),rt2=sum(rt2)),by=c("sex","agegroups")]
##' D[sex=="m",e1:=e1+rpois(.N,lambda=as.numeric(agegroups)*17)]
##' D[sex=="m",rt1:=rt1-rpois(.N,lambda=as.numeric(agegroups)*1600)]
##' dsr(count1=D[sex=="m",e1], pop1=D[sex=="m",rt1],
##' count0=D[sex=="f",e1], pop0=D[sex=="f",rt1],
##' stdpop=D[sex=="f",rt1])
##' @export 
##' @author Paul F Blanche  <pabl@@sund.ku.dk> and Thomas A. Gerds <tag@@biostat.ku.dk>
### Code:
dsr <- function(count1,
                pop1,
                count0,
                pop0,
                stdpop,
                conf.level = 0.95,
                method="gamma",
                crude=TRUE){
    alpha <- (1-conf.level)
    qalpha <- qnorm(1-alpha/2)
    ## {{{ point estimates
    w <- stdpop/sum(stdpop) # weights
    lambda1 <- count1/pop1 # rates for  group 1
    lambda0 <- count0/pop0 # rates for  group 1
    DSR1 <- sum(lambda1*w)
    DSR0 <- sum(lambda0*w)
    allDSR <- c(DSR0,DSR1)
    R1 <- sum(count1)/sum(pop1)
    R0 <- sum(count0)/sum(pop0)
    crudeRatio <- R1/R0
    DSRRatio <- DSR1/DSR0
    ## }}}
    ## {{{ exact CI for crude rates
    if (tolower(method)%in%c("gamma")){
        # as (from)  poisson.test (except alpha -> alpha/2 inside function)
        p.L <- function(x, alpha) {
            if (x == 0) 
                0
            else stats::qgamma(alpha/2, x)
        }
        p.U <- function(x, alpha) stats::qgamma(1 - alpha/2, x + 1)
        crude.lower=c(p.L(sum(count0),alpha)/sum(pop0),p.L(sum(count1),alpha)/sum(pop1))
        crude.upper=c(p.U(sum(count0),alpha)/sum(pop0),p.U(sum(count1),alpha)/sum(pop1))
        ## }}}
    }
    ## {{{ compute raw (plain) Wald interval of each crude rates
    if (substr(tolower(method),0,4)=="wald"){
        varR1 <- sum(count1)/sum(pop1)^2 # usual formula for poisson
        varR0 <- sum(count0)/sum(pop0)^2
        if (tolower(method)=="wald.log"){
            crude.lower=exp(c(log(R0)-qalpha*sqrt(varR0)/R0,log(R1)-qalpha*sqrt(varR1)/R1))
            crude.upper=exp(c(log(R0)+qalpha*sqrt(varR0)/R0,log(R1)+qalpha*sqrt(varR1)/R1))
        }else{
            crude.lower=c(R0-qalpha*sqrt(varR0),R1-qalpha*sqrt(varR1))
            crude.upper=c(R0+qalpha*sqrt(varR0),R1+qalpha*sqrt(varR1))
        }
    }
    ## }}}

    ## {{{ compute raw (plain) Wald interval of each DSR
    if (substr(tolower(method),0,4)=="wald"){
        # estimator of the variance of the Directly standardized Rates
        varDSR1 <- sum(count1*(w/pop1)^2)
        varDSR0 <- sum(count0*(w/pop0)^2)
        if (tolower(method)=="wald.log"){
            varlogDSR0 <- varDSR0*(1/DSR0)^2
            varlogDSR1 <- varDSR1*(1/DSR1)^2
            std.lower <- c(exp(log(DSR0) - qalpha*sqrt(varlogDSR0)), exp(log(DSR1) - qalpha*sqrt(varlogDSR1)))
            std.upper <- c(exp(log(DSR0) + qalpha*sqrt(varlogDSR0)), exp(log(DSR1) + qalpha*sqrt(varlogDSR1)))
        }else{
            std.lower <- c(DSR0 - qalpha*sqrt(varDSR0),DSR1 - qalpha*sqrt(varDSR1))
            std.upper <- c(DSR0 + qalpha*sqrt(varDSR0),DSR1 + qalpha*sqrt(varDSR1))
        }
    }
    ## }}}    

    ## Gamma CI for each DSR
    # similar to  epitools::ageadjust.direct
    CIgamma <- function(alpha){
        wM1 <- max(w/pop1)
        wM0 <- max(w/pop0)
        varDSR1 <- sum(count1*(w/pop1)^2)
        varDSR0 <- sum(count0*(w/pop0)^2)
        lci.0 <- stats::qgamma(alpha/2,
                               shape = (DSR0^2)/varDSR0,
                               scale = varDSR0/DSR0)
        uci.0 <- stats::qgamma(1 - alpha/2,
                               shape = ((DSR0 + wM0)^2)/(varDSR0 + wM0^2),
                               scale = (varDSR0 + wM0^2)/(DSR0 + wM0))
        lci.1 <- stats::qgamma(alpha/2,
                               shape = (DSR1^2)/varDSR1,
                               scale = varDSR1/DSR1)
        uci.1 <- stats::qgamma(1 - alpha/2,
                               shape = ((DSR1 + wM1)^2)/(varDSR1 + wM1^2),
                               scale = (varDSR1 + wM1^2)/(DSR1 + wM1))   
        gammaCIDSR <- rbind(c(lci.0,uci.0),
                            c(lci.1,uci.1))
        colnames(gammaCIDSR) <- c("lower","upper")
        rownames(gammaCIDSR) <- c("group 0","group 1")
        gammaCIDSR
    }
    if (tolower(method)=="gamma"){
        CIDSR <- CIgamma(alpha)
        std.lower <- CIDSR[,"lower"]
        std.upper <- CIDSR[,"upper"]
    }
    ## Wald CI for crude and directly standardized Rate Ratio

    ## Wald (Delta method)
    if (substr(tolower(method),0,4)=="wald"){
        # Here we compute the CI by using a delta method. We first compute
        # the var of the log of the ratios (some of the var of the log estimates).
        # Then we build a CI on the log scale and then we backtransform.
        varlogCrudeRatio <- varR1/(R1^2) + varR0/(R0^2)
        varlogDSRRatio <- varDSR1/(DSR1^2) + varDSR0/(DSR0^2)
        crude.rr.lower = exp(log(crudeRatio) - qalpha*sqrt(varlogCrudeRatio))
        crude.rr.upper = exp(log(crudeRatio) + qalpha*sqrt(varlogCrudeRatio))
        std.rr.lower = exp(log(DSRRatio) - qalpha*sqrt(varlogDSRRatio))
        std.rr.upper = exp(log(DSRRatio) + qalpha*sqrt(varlogDSRRatio))
    }
    ## Exact CI for crude ratio
    # we just call poisson.test, which calls binom.test, see code of poisson.test
    # or e.g. Fay R Journal 2010.
    if (tolower(method)!="wald"){
        CI.crude.Ratio <- as.vector(stats::poisson.test(x=c(sum(count1),sum(count0)),T=c(sum(pop1),sum(pop0)))$conf.int)
        crude.rr.lower <- CI.crude.Ratio[1]
        crude.rr.upper <- CI.crude.Ratio[2]
    }
    ## using the inverse of the F distribution
    if (tolower(method)=="gamma"){
        w1 <- w/pop1
        w0 <- w/pop0
        index1 <- which(count1<(count1+count0))
        index0 <- which(count0<(count1+count0))
        wMRR1 <- max(w1[index1])
        wMRR0 <- max(w0[index0])
        varDSR1 <- sum(count1*(w/pop1)^2)
        varDSR0 <- sum(count0*(w/pop0)^2)
        nu1 <- (2*DSR1^2)/varDSR1
        nu0 <- (2*DSR0^2)/varDSR0
        nu1star <- (2*(DSR1 + wMRR1)^2)/(varDSR1+wMRR1^2)
        nu0star <- (2*(DSR0 + wMRR0)^2)/(varDSR0+wMRR0^2)
        std.rr.lower <- (DSR1 / (DSR0 + wMRR0) )*stats::qf(p=alpha/2, df1=nu1, df2=nu0star)
        std.rr.upper <- ((DSR1 + wMRR1)/DSR0)*stats::qf(p=1-alpha/2, df1=nu1star, df2=nu0)
    }
    out <- data.table(group=c(0,1),
                      rate=c(DSR0,DSR1),
                      rate.lower=std.lower,
                      rate.upper=std.upper,
                      ratio=c(1,DSRRatio),
                      ratio.lower=c(1,std.rr.lower),
                      ratio.upper=c(1,std.rr.upper))
    if (crude) {
        out <- cbind(type="standardized",out)
        out <- rbindlist(list(out,data.table(type="crude",group=c(0,1),
                                             rate=c(R0,R1),
                                             rate.lower=crude.lower,
                                             rate.upper=crude.upper,
                                             ratio=c(1,crudeRatio),
                                             ratio.lower=c(1,crude.rr.lower),
                                             ratio.upper=c(1,crude.rr.upper))))
    }
    out[]
}

##----------------------------------------------------------------------
### dsr.R ends here
