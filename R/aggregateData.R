#' Aggregate data for poisson regression
#' 
#' @title Aggregation of data - useful for poisson regression
#' @param data dataset containing the variables "interval" and "event".
#' @param timegrid vector of times the data is aggregated by
#' @export
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

