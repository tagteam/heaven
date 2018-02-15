library(lava)
library(data.table)
library(Publish)

######################################
# Does rates equal the ones simulated
######################################

# if data is without censoring, and only theta0 is included in simulated data, rates
# in glm should match the baselinerates we used for construction of simulated data

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
fit1 <- poissonregression(formula=event~-1+X*Z+time+offset(log(risktime)),data=simdat,
                         timegrid=c(1,2))

aggregateData <- function(data,timegrid){
  if (class(data)[1]!="data.table") data <- data.table(data)
  data[,time:=interval]
  if (length(timegrid)<=length(levels(data$time))){
    levels(data$time) <- timegrid
    if (match("V",names(data),nomatch=FALSE))
      aggdata <- data[,list(risktime=sum(risktime),event=sum(event)),by=list(X,Z,V,time)]
    else
      aggdata <- data[,list(risktime=sum(risktime),event=sum(event)),by=list(X,Z,time)]
    return(aggdata)
  }else
    return(data)
}

poissonregression <- function(formula,data,timegrid,effectZgrid){
  aggdata <- aggregateData(data=data,timegrid=timegrid)
  if (!missing(effectZgrid)){
    aggdata[,Ztime:=factor(time)]
    levels(aggdata$Ztime) <- effectZgrid
  }
  fit <- glm(formula,data=aggdata,family=poisson())
  fit
}

# Rates simulated: 0.005, 0.02
testdat <- aggregateData(simdat,1:2)

# Relative frequencies 

# First time interval
testdat[X==0&Z==0&time==1,event]/testdat[X==0&Z==0&time==1,risktime]
# Second timeinterval
testdat[X==0&Z==0&time==2,event]/testdat[X==0&Z==0&time==2,risktime]

######################################
# Does rates act as expected when beta is included in simulated data
######################################

test.beta <- function(b,theta0=rep(0.005,2),n=10000,ints=c(1:2),time.p=1){
simdat2 <- simpoisson(N=n,
                     changepoints=ints,
                     baseline=theta0,
                     beta=log(b),
                     gamma=rep(0,length(ints)),
                     px=0.4,
                     pz=0.3,
                     pxz=0,
                     const="XZ")

fit2 <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=simdat2,
                         timegrid=ints)

return(list(exp(coef(fit2)),poissonRisk(fit2,interval=1,
            newdata=expand.grid(X=0:1,Z=0:1,time=factor(time.p,levels=ints)))))
}

test.beta(rep(1,2))[[2]] 
test.beta(rep(0.5,2))[[2]]
test.beta(rep(2,2))[[2]]

######################################
# Test interactions
######################################

fita <- poissonregression(formula=event~-1+X+Z+time+offset(log(risktime)),data=simdat,
                         timegrid=c(1,2))
fita1 <- poissonregression(formula=event~-1+X*Z+time+offset(log(risktime)),data=simdat,
                          timegrid=c(1,2))

a <- poissonRisk(fita,formula=~-1+X+Z+time,interval=1,
                 newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2))))
a[2,4]/a[1,4]
a[4,4]/a[3,4]

a1 <- poissonRisk(fita1,formula=~-1+X*Z+time,interval=1,
                  newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2))))
a1[2,4]/a1[1,4]
a1[4,4]/a1[3,4]

######################################
# Test if model can be fitted without offset
######################################

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

poissonRisk(fit.test,interval=1,
            newdata=expand.grid(X=0:1,Z=0:1,time=factor(1,levels=c(1,2))))

######################################
# Increase number of intervals
######################################

t1 <- test.beta(b=rep(1,length(c(15:50))),
          n=10000,theta0 = rep(0.005,length(c(15:50))),
          ints=c(15:50),
          time.p=30)

t1[[2]]

# Add effect of X
t1 <- test.beta(b=rep(3,length(c(15:50))),
                n=10000,theta0 = rep(0.005,length(c(15:50))),
                ints=c(15:50),
                time.p=35)

t1[[2]]
