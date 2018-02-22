### RateRatioCI.R --- 
##----------------------------------------------------------------------
## Author: Paul Blanche
## Created: Feb  2 2018 (15:02) 
## Version: 
## Last-Updated: Feb 22 2018 (15:10) 
##           By: Paul Blanche
##     Update #: 345
##----------------------------------------------------------------------
## 
### Commentary:
##
## Function to Compute confidence interval for directly standardized rates
## and rate ratios for sparse data. Method implemented include gamma confidence
## intervals (for DSR), exact confidence intervals (for crude rates),
## the inverse of the F distribution (for DSR ratio), melted confidence (for DSR ratios)
## and some Wald confidence interval (also on log-scale) for comparison purpose.
## The methods are described in:
##
## Fay, Michael P., and Eric J. Feuer. "Confidence intervals for directly
## standardized rates: a method based on the gamma distribution." Statistics
## in Medicine 16.7 (1997): 791-801.
##
## Fay, Michael P. "Approximate confidence intervals for rate ratios from
## directly standardized rates with sparse data." Communications in
## Statistics-Theory and Methods 28.9 (1999): 2141-2160.
##
## Fay, Michael P., et al. "Estimating average annual percent change
## for disease rates without assuming constant change." Biometrics
## 62.3 (2006): 847-854.
##
## Fay, Michael P., Michael A. Proschan, and Erica Brittain. "Combining
## one‐sample confidence procedures for inference in the two‐sample case."
## Biometrics 71.1 (2015): 146-156.
##
## Interesting other approaches, to avaoid inconsistencies between
## exact CI and p-values are implemented in exactci and exact2x2, see
##
## Fay, Michael P. "Two-sided exact tests and matching confidence intervals
## for discrete data." R journal 2.1 (2010): 53-58.
##
## Input:
## 
## count1,            # counts for group 1 (e.g. exposed)
## pop1,              # population for group 1 
## count0,            # counts for group 0 (e.g. non exposed)
## pop0,              # population for group 0
## stdpop             # standard population to standardize aaccordingly
## conf.level = 0.95  # confidence level of confidence intervals
## NMC=50000          # number of Monte Carlo simulation to compute Melted intervals
## seed=1234          # seed for reproducibility of melted confidence intervals
##
## Ouput:
##
## crude.Rates                : crude rates
## DSR                        : DSR, i.e. Directly Standardized Rates
## exact.CI.crude.Rates       : Exact confidence intervals for crude rates
## raw.Wald.CI.crude.Rates    : raw Wald confidence intervals for crude rates
## log.Wald.CI.crude.Rates    : Wald confidence intervals for crude rates, using the log for the normal approximation
## raw.Wald.CI.DSR            : raw Wald confidence intervals for DSR
## log.Wald.CI.DSR            : Wald confidence intervals for DSR, using the log for the normal approximation
## gamma.CI.DSR               : Gamma confidence intervals for DSR
## crude.Ratio                : crude rates ratio
## log.Wald.CI.crude.Ratio    : Wald confidence intervals for the crude rate ratio, using the log for the normal approximation
## exact.CI.crude.Ratio       : Exact confidence intervals for the crude rate ratio
## DSR.Ratio                  : DSR ratio
## log.Wald.CI.DSR.Ratio      : Wald confidence intervals for the DSR ratio, using the log for the normal approximation
## F.dist.CI.DSR.Ratio        : F distribution based confidence intervals for the DSR ratio
## melted.CI.DSR.Ratio        : Melted gamma confidence intervals for the DSR ratio
##
## ----------------------------------------------------------------------
## 
### Code:

RateRatioCI <- function(count1, # counts for group 1 
                        pop1,   # population for group 1 
                        count0, # counts for group 0 (e.g. non exposed)
                        pop0,   # population for group 0
                        stdpop, conf.level = 0.95,
                        NMC=50000,
                        seed=1234){
    set.seed(seed)
    ## {{{ preliminaries
    alpha <- (1-conf.level)
    w <- stdpop/sum(stdpop) # weights

    lambda1 <- count1/pop1 # rates for  group 1
    lambda0 <- count0/pop0 # rates for  group 1

    DSR1 <- sum(lambda1*w)
    DSR0 <- sum(lambda0*w)
    allDSR <- c(DSR0,DSR1)
    R1 <- sum(count1)/sum(pop1)
    R0 <- sum(count0)/sum(pop0)
    crudeRates <- c(R0,R1)
    names(allDSR) <- c("group 0","group 1")

    crudeRatio <- R1/R0
    DSRRatio <- DSR1/DSR0
    names(crudeRatio) <- "RR (1 vs0)"
    names(DSRRatio) <- "RR (1 vs0)"

    qalpha <- qnorm(1-alpha/2)
    ## }}}

    ## {{{ exact CI for crude rates
    # as (from)  poisson.test (excetp alpha -> alpha/2 inside function)
    p.L <- function(x, alpha) {
        if (x == 0) 
            0
        else qgamma(alpha/2, x)
    }
    p.U <- function(x, alpha) qgamma(1 - alpha/2, x + 1)
    ## browser()
    CIcrude <- rbind(c(p.L(sum(count0),alpha),p.U(sum(count0),alpha))/sum(pop0),
                     c(p.L(sum(count1),alpha),p.U(sum(count1),alpha))/sum(pop1)
                     )
    colnames(CIcrude) <- c("lower","upper")
    rownames(CIcrude) <- c("group 0","group 1")
    ## }}}

    ## {{{ compute raw (plain) Wald interval of each crude rates
    varR1 <- sum(count1)/sum(pop1)^2 # usual formula for poisson
    varR0 <- sum(count0)/sum(pop0)^2
    WaldCIcrude <- rbind(c(R0-qalpha*sqrt(varR0), R0+qalpha*sqrt(varR0)),
                         c(R1-qalpha*sqrt(varR1), R1+qalpha*sqrt(varR1)))
    colnames(WaldCIcrude) <- c("lower","upper")
    rownames(WaldCIcrude) <- c("group 0","group 1")
    ## }}}

    ## {{{ Compute Wald interval of each crude rate using log transformation

    logWaldCIcrude <- exp(rbind(c(log(R0)-qalpha*sqrt(varR0)/R0,
                                  log(R0)+qalpha*sqrt(varR0)/R0),
                                c(log(R1)-qalpha*sqrt(varR1)/R1,
                                  log(R1)+qalpha*sqrt(varR1)/R1)))
    colnames(logWaldCIcrude) <- c("lower","upper")
    rownames(logWaldCIcrude) <- c("group 0","group 1")
    ## }}}
    
    ## {{{ compute raw (plain) Wald interval of each DSR

    # estimator of the variance of the Directly standardized Rates
    varDSR1 <- sum(count1*(w/pop1)^2)
    varDSR0 <- sum(count0*(w/pop0)^2)

    lowerWald0 <- DSR0 - qalpha*sqrt(varDSR0)
    upperWald0 <- DSR0 + qalpha*sqrt(varDSR0)
    lowerWald1 <- DSR1 - qalpha*sqrt(varDSR1)
    upperWald1 <- DSR1 + qalpha*sqrt(varDSR1)
    
    WaldCIDSR <- rbind(c(lowerWald0,upperWald0),
                       c(lowerWald1,upperWald1))
    colnames(WaldCIDSR) <- c("lower","upper")
    rownames(WaldCIDSR) <- c("group 0","group 1")
    ## }}}    

    ## {{{ Compute Wald interval of each DSR on using log transformation
    varlogDSR0 <- varDSR0*(1/DSR0)^2
    varlogDSR1 <- varDSR1*(1/DSR1)^2

    lowerlogWald0 <- exp(log(DSR0) - qalpha*sqrt(varlogDSR0))
    upperlogWald0 <- exp(log(DSR0) + qalpha*sqrt(varlogDSR0))
    lowerlogWald1 <- exp(log(DSR1) - qalpha*sqrt(varlogDSR1))
    upperlogWald1 <- exp(log(DSR1) + qalpha*sqrt(varlogDSR1))

    
    logWaldCIDSR <- rbind(c(lowerlogWald0,upperlogWald0),
                          c(lowerlogWald1,upperlogWald1))
    colnames(logWaldCIDSR) <- c("lower","upper")
    rownames(logWaldCIDSR) <- c("group 0","group 1")
    ## }}}   

    ## {{{ Gamma CI for each DSR
    # similar to  epitools::ageadjust.direct
    CIgamma <- function(alpha){
        wM1 <- max(w/pop1)
        wM0 <- max(w/pop0)
        lci.0 <- qgamma(alpha/2,
                        shape = (DSR0^2)/varDSR0,
                        scale = varDSR0/DSR0)
        uci.0 <- qgamma(1 - alpha/2,
                        shape = ((DSR0 + wM0)^2)/(varDSR0 + wM0^2),
                        scale = (varDSR0 + wM0^2)/(DSR0 + wM0))
        lci.1 <- qgamma(alpha/2,
                        shape = (DSR1^2)/varDSR1,
                        scale = varDSR1/DSR1)
        uci.1 <- qgamma(1 - alpha/2,
                        shape = ((DSR1 + wM1)^2)/(varDSR1 + wM1^2),
                        scale = (varDSR1 + wM1^2)/(DSR1 + wM1))   
        gammaCIDSR <- rbind(c(lci.0,uci.0),
                            c(lci.1,uci.1))
        colnames(gammaCIDSR) <- c("lower","upper")
        rownames(gammaCIDSR) <- c("group 0","group 1")
        gammaCIDSR
    }

    gammaCIDSR <- CIgamma(alpha)

    ## }}}

    ## {{{ Wald CI for crude and directly standardized Rate Ratio

    ## {{{ Wald (Delta method)
    # Here we compute the CI by using a delta method. We first compute
    # the var of the log of the ratios (some of the var of the log estimates).
    # Then we build a CI on the log scale and then we backtransform.

    varlogCrudeRatio <- varR1/(R1^2) + varR0/(R0^2)
    varlogDSRRatio <- varDSR1/(DSR1^2) + varDSR0/(DSR0^2)

    
    ## raw.Wald.CI.crude.Ratio <- c(NA,NA)
    log.Wald.CI.crude.Ratio <- exp(c(log(crudeRatio) - qalpha*sqrt(varlogCrudeRatio),
                                     log(crudeRatio) + qalpha*sqrt(varlogCrudeRatio)))

    ## raw.Wald.CI.DSR.Ratio <- c(NA,NA)
    log.Wald.CI.DSR.Ratio <- exp(c(log(DSRRatio) - qalpha*sqrt(varlogDSRRatio),
                                   log(DSRRatio) + qalpha*sqrt(varlogDSRRatio)))
    names(log.Wald.CI.DSR.Ratio) <- c("lower","upper")
    ## }}}

    ## {{{ Exact CI for crude ratio
    # we just call poisson.test, which calls binom.test, see code of poisson.test
    # or e.g. Fay R Journal 2010.
    exact.CI.crude.Ratio <- as.vector(poisson.test(x=c(sum(count1),
                                                       sum(count0)),
                                                   T=c(sum(pop1),
                                                       sum(pop0)))$conf.int)
    names(exact.CI.crude.Ratio) <- c("lower","upper")
    ## }}}

    ## {{{ using the inverse of the F distribution
    ## browser()
    w1 <- w/pop1
    w0 <- w/pop0

    index1 <- which(count1<(count1+count0))
    index0 <- which(count0<(count1+count0))

    wMRR1 <- max(w1[index1])
    wMRR0 <- max(w0[index0])
    
    nu1 <- (2*DSR1^2)/varDSR1
    nu0 <- (2*DSR0^2)/varDSR0

    nu1star <- (2*(DSR1 + wMRR1)^2)/(varDSR1+wMRR1^2)
    nu0star <- (2*(DSR0 + wMRR0)^2)/(varDSR0+wMRR0^2)

    lowerF <- (DSR1 / (DSR0 + wMRR0) )*qf(p=alpha/2, df1=nu1, df2=nu0star)
    upperF <- ((DSR1 + wMRR1)/DSR0)*qf(p=1-alpha/2, df1=nu1star, df2=nu0)

    F.dist.CI.DSR.Ratio <- c(lowerF,upperF)
    names(F.dist.CI.DSR.Ratio) <- c("lower","upper")
    ## }}}

    ## {{{ melted Gamma intervals
    
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

    melted.CI.DSR.Ratio <- c(quantile(gGreater, probs = alpha),
                             quantile(gLess, probs = 1 - alpha))
    names(melted.CI.DSR.Ratio) <- c("lower","upper")
    ## }}}
    
    ## }}}

    ## browser()
    ## {{{ create output
    out <- list(crude.Rates=crudeRates,
                DSR=allDSR,
                exact.CI.crude.Rates=CIcrude,
                raw.Wald.CI.crude.Rates=WaldCIcrude,
                log.Wald.CI.crude.Rates=logWaldCIcrude,
                raw.Wald.CI.DSR=WaldCIDSR,
                log.Wald.CI.DSR=logWaldCIDSR,
                gamma.CI.DSR=gammaCIDSR,
                crude.Ratio=crudeRatio,
                log.Wald.CI.crude.Ratio=log.Wald.CI.crude.Ratio,
                exact.CI.crude.Ratio=exact.CI.crude.Ratio,
                DSR.Ratio=DSRRatio,
                log.Wald.CI.DSR.Ratio=log.Wald.CI.DSR.Ratio,
                F.dist.CI.DSR.Ratio=F.dist.CI.DSR.Ratio,
                melted.CI.DSR.Ratio=melted.CI.DSR.Ratio
                )
    ## }}}
    
    out
}

##----------------------------------------------------------------------
### RateRatioCI.R ends here
