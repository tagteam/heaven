#' @title Poisson regression by aggregated data using glm
#' @param formula The formula as specified in glm.
#' @param data dataset containing the variables "interval" and "event".
#' @param timegrid vector of times the data should be aggregated by.
#' @param effectZgrid
#' @export

poissonregression <- function(formula,data,timegrid,effectZgrid){
  aggdata <- aggregateData(data=data,timegrid=timegrid)
  if (!missing(effectZgrid)){
    aggdata[,Ztime:=factor(time)]
    levels(aggdata$Ztime) <- effectZgrid
  }
  fit <- glm(formula,data=aggdata,family=poisson())
  fit
}

