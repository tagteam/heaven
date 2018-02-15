#' @title Simulate data for poisson regression
#' @description Simulate from piecewise constant exponential regression model
#' with two binary variables X, Z whose distributions and effects
#' may vary in time intervals. 
#' @param N sample size.
#' @param changepoints borders of time intervals.
#' @param baseline baseline rate in intervals.
#' @param beta effect of X (log hazard ratio).
#' @param gamma effect of Z (log hazard ratio).
#' @param delta effect of V (log hazard ratio).
#' @param comprisk rate of competing risk event.
#' @param alpha effect of X on competing risk (log hazard ratio).
#' @param zeta effect of Z on competing risk (log hazard ratio).
#' @param px probability of X in intervals.
#' @param pz probability of Z in intervals.
#' @param pv probability vector for categories of V.
#' @param pxz effect of Z on X (log odds ratio).
#' @param const Whether the distributions of X and Z 
#' vary over time is controlled with argument `const'.
#' const="XZ" means that both X and Z do not change over time.
#' const="X" means that only X does not change over time.
#' const="Z" means that only Z does not change over time.
#' @export
#' @author Thomas A. Gerds <tag@biostat.ku.dk>
simpoisson <- function(N,            # sample size
                       changepoints, # borders of time intervals
                       baseline,     # baseline rate in intervals
                       beta,         # effect of X (log hazard ratio)
                       gamma,        # effect of Z (log hazard ratio)
                       delta=0,      # effect of V (log hazard ratio)
                       comprisk,     # rate of competing risk event
                       alpha,        # effect of X on competing risk (log hazard ratio)
                       zeta,         # effect of Z on competing risk (log hazard ratio)
                       px,           # probability of X in intervals
                       pz,           # probability of Z in intervals
                       pv=NULL,      # probability vector for categories of V
                       pxz=0,        # effect of Z on X (log odds ratio)
                       const="none") # see above
{
  require(data.table)
  require(lava)
  changepoints <- sort(changepoints)
  NT <- length(changepoints) # number of time intervals
  stopifnot(all(c(length(baseline),length(beta),length(gamma))==NT))
  if (missing(comprisk)) comprisk <- NULL
  else stopifnot(length(comprisk)==NT)
  changepoints <- c(0,changepoints)
  width <- diff(changepoints)   # width of time intervals
  m0 <- lava::lvm()
  if (!is.null(pv))
    m0 <- categorical(m0,~V,,K=length(pv)+1,p=pv)
  switch(const,
         "X"={distribution(m0,~X) <- binomial.lvm(p=px[1])},
         "Z"={distribution(m0,~Z) <- binomial.lvm(p=pz[1])},
         "XZ"={
           distribution(m0,~X) <- binomial.lvm(p=px[1])
           distribution(m0,~Z) <- binomial.lvm(p=pz[1])
           regression(m0,X~Z) <- pxz[1]
         },
         # anything else
         {stopifnot(all(c(length(px),length(pz),length(pxz))==NT))
           const <- "none"})
  if (const!="none"){
    startdata <- sim(m0,N)
  }
  ## simulate data within time interval, independent of the previous interval
  interval.data <- lapply(1:NT,function(t){
    m <- lava::lvm()
    switch(const,
           "X"={ 
             distribution(m,~X) <- list(startdata$X)
             distribution(m,~Z) <- binomial.lvm(p=pz[t])},
           "Z"={
             distribution(m,~X) <- binomial.lvm(p=px[t])
             distribution(m,~Z) <- list(startdata$Z)
             regression(m,X~Z) <- pxz[t]},
           "XZ"={
             distribution(m,~X) <- list(startdata$X)
             distribution(m,~Z) <- list(startdata$Z)},
           ## any other value
           {distribution(m,~X) <- binomial.lvm(p=px[t])
           distribution(m,~Z) <- binomial.lvm(p=pz[t])
           regression(m,X~Z) <- pxz[t]
           })
    if (!is.null(pv))
      distribution(m,~V) <- list(startdata$V)
    distribution(m,"eventtime") <- coxExponential.lvm(scale=baseline[t])
    ## cut time at interval border
    distribution(m,"censtime") <- sequence.lvm(width[t],width[t])
    regression(m,eventtime~X) <- beta[t]
    regression(m,eventtime~Z) <- gamma[t]
    if (!is.null(pv))
      regression(m,eventtime~V) <- delta
    if (length(comprisk)>0){
      distribution(m,"crtime") <- coxExponential.lvm(scale=comprisk[t])
      regression(m,crtime~X) <- alpha[t]
      regression(m,crtime~Z) <- zeta[t]
      m <- eventTime(m,risktime~min(eventtime=1,censtime=0,crtime=2),"event")
    }
    else
      m <- eventTime(m,risktime~min(eventtime=1,censtime=0),"event")
    if (!is.null(pv))
      data.table(cbind(sim(m,N)[,c("X","Z","V","risktime","event")],id=1:N,interval=t,label=paste(changepoints[t],"-",changepoints[t+1])))
    else
      data.table(cbind(sim(m,N)[,c("X","Z","risktime","event")],id=1:N,interval=t,label=paste(changepoints[t],"-",changepoints[t+1])))
  })
  d <- rbindlist(interval.data)
  setkey(d,id,interval)
  d[,toolate:=cumsum(cumsum(event)),by=id]
  if (!is.null(pv))
    d <- d[toolate<=1,list(X,Z,V,event,risktime,interval,label)]
  else
    d <- d[toolate<=1,list(X,Z,event,risktime,interval,label)]
  d[,interval:=factor(interval)]
  d
}