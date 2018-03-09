library(lava)
library(data.table)
library(Publish)
library(foreach)
library(doParallel)

######################################
# Does rates equal the ones simulated
######################################

# if data is without censoring, and only theta0 is included in simulated data, rates
# in glm should match the baselinerates we used for construction of simulated data

test_that("Simulated rates vs. estimated rates",{
set.seed(28587)
simdat <- simpoisson(N=1000,
                     changepoints=c(1,2),
                     baseline=c(0.005,0.02),
                     beta=log(c(1,1)),
                     gamma=rep(0,2),
                     px=0.4,
                     pz=0.3,
                     pxz=0,
                     const="XZ")


fit <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=simdat,
                         timegrid=c(1,2))

# Rates simulated: 0.005, 0.02
testdat <- aggregateData(simdat,1:2)

## Relative frequencies 

# First time interval
rate1 <- testdat[X==0&Z==0&time==1,event]/testdat[X==0&Z==0&time==1,risktime]

# Second timeinterval
rate2 <- testdat[X==0&Z==0&time==2,event]/testdat[X==0&Z==0&time==2,risktime]

expect_equal(as.numeric(exp(fit$coefficients[3])),rate1,tolerance=.0002)
expect_equal(as.numeric(exp(fit$coefficients[4])),rate2,tolerance=.002)
})

############################################################################
# Does rates act as expected when beta is included in simulated data
############################################################################

test_that('Decrease and increase in rate by beta',{
test.beta <- function(b,theta0=rep(0.005,2),n=10000,ints=c(1:2),time.p=1){
simdat <- simpoisson(N=n,
                     changepoints=ints,
                     baseline=theta0,
                     beta=log(b),
                     gamma=rep(0,length(ints)),
                     px=0.4,
                     pz=0.3,
                     pxz=0,
                     const="XZ")

fit <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=simdat,
                         timegrid=ints)

return(list(exp(coef(fit)),poissonRisk(fit,interval=1,
            newdata=expand.grid(X=0:1,Z=0:1,time=factor(time.p,levels=ints)))))
}

expect_true(test.beta(rep(1,2))[[2]][2,4]>test.beta(rep(0.5,2))[[2]][2,4])

expect_true(test.beta(rep(1,2))[[2]][2,4]<test.beta(rep(2,2))[[2]][2,4])

})

######################################
# Test interactions
######################################

test_that('Interactions',{
fita <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=simdat,
                         timegrid=c(1,2))
fita1 <- poissonregression(formula=event~-1+X*Z+time+offset(log(risktime)),data=simdat,
                          timegrid=c(1,2))

a <- poissonRisk(fita,interval=1,
                 newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2))))

expect_equal(a[2,4]/a[1,4],a[4,4]/a[3,4],tolerance=0.002)

a1 <- poissonRisk(fita1,interval=1,
                  newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2))))

expect_true(round(a1[2,4]/a1[1,4],2)!=round(a1[4,4]/a1[3,4],2))
})

###############################################
# Test if model can be fitted without offset
###############################################

test_that('exclude offset',{
simdat <- simpoisson(N=1000,
                     changepoints=c(1,2),
                     baseline=c(0.005,0.02),
                     beta=log(c(1,1)),
                     gamma=rep(0,2),
                     px=0.4,
                     pz=0.3,
                     pxz=0,
                     const="XZ")

fit.test <- poissonregression(formula=event~-1+X+Z+time,data=simdat,
                                     timegrid=c(1,2))

expect_error(poissonRisk(fit.test,interval=1,
            newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2)))))
})

######################################
# Increase number of intervals
######################################

# t1 <- test.beta(b=rep(1,length(c(15:50))),
#           n=10000,theta0 = rep(0.005,length(c(15:50))),
#           ints=c(15:50),
#           time.p=30)
# 
# t1[[2]]
# 
# # Add effect of X
# t1 <- test.beta(b=rep(3,length(c(15:50))),
#                 n=10000,theta0 = rep(0.005,length(c(15:50))),
#                 ints=c(15:50),
#                 time.p=35)
# 
# t1[[2]]

##################################################################
# Testing standard errors from lava (robust vs. non-robust)
##################################################################

test_that('robust vs. non robust sd',{
# Test function with se option in lava estimate added
poissonRisk.test <- function(model,interval,newdata,se.robust){
  model.t <- terms(model)
  offset.position <- attr(model.t,'offset')
  if(is.null(offset.position)){stop('Offset is missing')}
  ff <- formula(drop.terms(model.t,offset.position))
  new.mat <- model.matrix(ff,newdata)
  out <- estimate(model,robust=se.robust,f=function(p){ 1-exp(-exp(new.mat%*%matrix(p))*interval)})
  out <- cbind(newdata,summary(out)$coefmat)
  out
}

test.lava <- function(dat,time.p=1,se.robust=TRUE){
  fit <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=dat,
                           timegrid=c(1:2))
  
  return(poissonRisk.test(fit,interval=1,se.robust, newdata=data.frame(X=1,Z=1,time=factor(time.p,levels=c(1:2)))))
}

# Simulates several datasets and computes the absolute risk
cl <- makeCluster(4)
registerDoParallel(cl)

est <- foreach(i=1:3000, .packages = 'heaven') %dopar% {
  simdat <- simpoisson(N=10000,
                       changepoints=c(1:2),
                       baseline=rep(0.005,2),
                       beta=log(rep(1.5,2)),
                       gamma=rep(0,2),
                       px=0.4,
                       pz=0.3,
                       pxz=0,
                       const="XZ")
  return(test.lava(dat=simdat,time.p=1)[,4])
}
stopCluster(cl)

est.sim <- unlist(est)

#########

simdat <- simpoisson(N=10000,
                     changepoints=c(1:2),
                     baseline=rep(0.005,2),
                     beta=log(c(1.5,1.5)),
                     gamma=rep(0,2),
                     px=0.4,
                     pz=0.3,
                     pxz=0,
                     const="XZ")

# Computes se of the absolute risk using lava with robust and non-robust se
est.lava <- test.lava(dat=simdat,time.p=1,se.robust=TRUE)
est.lava2 <- test.lava(dat=simdat,time.p=1,se.robust=FALSE)

###########

sd(est.sim) # standard deviation of absolute risks based on 3000 simulated datasets
est.lava[,5] # robust se lava
est.lava2[,5] # non-robust se lava

expect_equal(est.lava2[,5],sd(est.sim),tolerance=0.0003)
expect_equal(est.lava[,5],sd(est.sim),tolerance=0.0003)

})