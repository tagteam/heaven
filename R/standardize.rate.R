##' Standardize proportions and absolute risks to a given age distribution
##'
##' Standardize proportions and absolute risks to a given age
##' distribution
##' @title Standardize proportions and absolute risks to a given age
##'     distribution
##' @param vars Names of variable(s) which contain the rate(s) to be
##'     standardized.
##' @param agevar Name of categorical age variable
##' @param byvar Name(s) of categorical strata variable(s)
##' @param reference what reference population to use for standardization.
##' @param data Data set which contains all variables
##' @param level Confidence level
##' @param ... Not (yet) used
##' @return Data table with standardized rates
##' @seealso standardize.prodlim standardize.proportion
##' @examples
##' library(riskRegression)
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
##' standardize.rate(vars=list(c("e1","rt1")),
##'                  agevar="agegroups",byvar="sex",data=D,reference="f")
##' standardize.rate(vars=list(c("e1","rt1")),
##'                  agevar="agegroups",byvar="sex",data=D,reference="m")
##' standardize.rate(vars=list(c("e1","rt1")),
##'                  agevar="agegroups",byvar="sex",data=D,reference="mean")
##' standardize.rate(vars=list(c("e1","rt1"),c("e2","rt2")),
##'                  agevar="agegroups",byvar="sex",data=d)
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
standardize.rate <- function(vars,
                             agevar,
                             byvar,
                             reference,
                             data,
                             level=0.95,
                             ...){
    requireNamespace("data.table")
    .N=N=.SD=weight=NULL
    if (!is.factor(data[[byvar]])){
        data[[byvar]] <- factor(data[[byvar]])
    }
    if (missing(reference)) {
        reference <- levels(data[[byvar]])[1]
    }
    setDT(data)
    N <- NROW(data)
    out <- vector(length(vars),mode="list")
    for (v in 1:length(vars)){
        if (reference=="mean"){
            stdpop <- data[,mean(.SD[[1]]),.SDcols=vars[[v]][[2]],by=c(agevar)][[2]]
        } else{
            stdpop <- data[data[[byvar]]==reference][[vars[[v]][[2]]]]
        }
        print(stdpop/sum(stdpop))
        out[[v]] <- data[,{
            std <- epitools::ageadjust.direct(count=.SD[[1]],
                                              pop=.SD[[2]],stdpop=stdpop)
            .(crude.rate=std[[1]],adj.rate=std[[2]],lower=std[[3]],upper=std[[4]])
        },.SDcols=vars[[v]],by=c(byvar)]
    }
    out
}

##' Function to Compute confidence interval for directly standardized rates and rate ratios
##'
##' Function to Compute confidence interval for directly standardized rates
##' and rate ratios for sparse data. Method implemented include gamma confidence
##' intervals (for DSR), exact confidence intervals (for crude rates),
##' the inverse of the F distribution (for DSR ratio), melted confidence (for DSR ratios)
##' and some Wald confidence interval (also on log-scale) for comparison purpose.
##' @title Confidence intervals for age standardized rates and rate ratios
##' @param count1 counts for group 1 (e.g. exposed)
##' @param pop1 number of subjects of person-years in group 1 
##' @param count0 counts for group 1 (e.g. exposed)
##' @param pop0 number of subjects of person-years in group 0 
##' @param stdpop number of subjects of person-years in reference population 
##' @param conf.level confidence level of confidence intervals
##' @param method method for calculating confidence intervals 
##' @param NMC number of Monte Carlo simulation to compute Melted intervals
##' @param seed seed for reproducibility of melted confidence intervals
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
##'
##' crude.Rates                : crude rates
##' DSR                        : DSR, i.e. Directly Standardized Rates
##' exact.CI.crude.Rates       : Exact confidence intervals for crude rates
##' raw.Wald.CI.crude.Rates    : raw Wald confidence intervals for crude rates
##' log.Wald.CI.crude.Rates    : Wald confidence intervals for crude rates, using the log for the normal approximation
##' raw.Wald.CI.DSR            : raw Wald confidence intervals for DSR
##' log.Wald.CI.DSR            : Wald confidence intervals for DSR, using the log for the normal approximation
##' gamma.CI.DSR               : Gamma confidence intervals for DSR
##' crude.Ratio                : crude rates ratio
##' log.Wald.CI.crude.Ratio    : Wald confidence intervals for the crude rate ratio, using the log for the normal approximation
##' exact.CI.crude.Ratio       : Exact confidence intervals for the crude rate ratio
##' DSR.Ratio                  : DSR ratio
##' log.Wald.CI.DSR.Ratio      : Wald confidence intervals for the DSR ratio, using the log for the normal approximation
##' F.dist.CI.DSR.Ratio        : F distribution based confidence intervals for the DSR ratio
##' melted.CI.DSR.Ratio        : Melted gamma confidence intervals for the DSR ratio
##'
##' @seealso epitools::ageadjust.direct
##' @examples
##' library(riskRegression)
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
                NMC=50000,
                seed=1234){
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
    if (substr(tolower(method),0,4)=="wald"|tolower(method)=="f"){
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
    if (substr(tolower(method),0,4)=="wald"|tolower(method)=="f"){
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
        std.lower <- CIDSR[1]
        std.upper <- CIDSR[2]
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
    if (tolower(method)=="f"){
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

    ## melted Gamma intervals
    if (tolower(method)=="gamma"){
        set.seed(seed)
        allsimu <- lapply(1:NMC,function(x){CIgamma(runif(1))})
        ## allsimu
        alllci0 <- unlist(lapply(allsimu,"[",1))
        alluci0 <- unlist(lapply(allsimu,"[",3))
        alllci1 <- unlist(lapply(allsimu,"[",2))
        alluci1 <- unlist(lapply(allsimu,"[",4))
        gGreater <- alllci1/alluci0
        gGreater[is.na(gGreater)] <- Inf   
        gLess <- alluci1/alllci0   
        gLess[is.na(gLess)] <- 0
        ## pgr <- length(gGreater[gGreater <= 1])/NMC
        ## pless <- length(gLess[gLess >= 1])/NMC
        ## pvalueMelted <- min(1, 2 * pgr, 2 * pless)
        CI.DSR.Ratio <- c(stats::quantile(gGreater, probs = alpha),
                          stats::quantile(gLess, probs = 1 - alpha))
        std.rr.lower <- CI.DSR.Ratio[1]
        std.rr.upper <- CI.DSR.Ratio[2]
    }
    out <- list(crude=data.table(group=c(0,1),
                                 rate=c(R0,R1),
                                 rate.lower=crude.lower,
                                 rate.upper=crude.upper,
                                 ratio=c(1,crudeRatio),
                                 ratio.lower=c(1,crude.rr.lower),
                                 ratio.upper=c(1,crude.rr.upper)),
                standardized=data.table(group=c(0,1),
                                        rate=c(DSR0,DSR1),
                                        rate.lower=std.lower,
                                        rate.upper=std.upper,
                                        ratio=c(1,DSRRatio),
                                        ratio.lower=c(1,std.rr.lower),
                                        ratio.upper=c(1,std.rr.upper)))
    out[]
}

##----------------------------------------------------------------------
### dsr.R ends here
