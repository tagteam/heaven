#' Aggregates data and performs poisson regression using GLM.
#' @title Poisson regression by aggregated data using glm
#' @param formula The formula as specified in glm.
#' @param data dataset.
#' @param aggvars vector of variables in data to be aggregated (summarized).
#' @param byvars vector of variables in data to aggregate by.
#' @return An object of class \code{c("glm", "lm")}: the fitted Poisson
#' regression model after summing \code{aggvars} within groups defined by
#' \code{byvars}. It contains coefficients, fitted values and model diagnostics
#' and can be used with \code{summary} and \code{predict}.
#' @export
poissonregression <- function(formula,data,aggvars,byvars){
    .SD=NULL
    if (class(data)[1]!="data.table") data <- data.table(data)
    aggdata <- data[,lapply(.SD,sum),by=byvars,.SDcols=aggvars]
    fit <- stats::glm(formula,data=aggdata,family=stats::poisson())
    fit
}
