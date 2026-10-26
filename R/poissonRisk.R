#' Predicting absolute risks from poisson regression
#' 
#' @title Predicting absolute risk from poisson regression model
#' @param model glm object of fitted poisson model
#' @param interval risk interval within time scale of the poisson model 
#' @param newdata values of the covariates in the poisson model for which the absolute risks are calculated.
#' @return A data frame (or data.table when \code{newdata} is a data.table)
#' with one row per row of \code{newdata}. The input covariates are followed
#' by the coefficient summary from \code{lava::estimate}, containing the
#' estimated absolute risk over \code{interval}, its standard error,
#' confidence limits and test statistics.
#' @export
#' @author Thomas A. Gerds <tag@biostat.ku.dk>, Regitze Kuhr Skals <r.skals@rn.dk>
poissonRisk <- function(model,interval,newdata){
  model.t <- stats::terms(model)
  offset.position <- attr(model.t,'offset')
  if(is.null(offset.position)){stop('Offset is missing')}
  ff <- stats::formula(stats::drop.terms(model.t,offset.position))
  new.mat <- stats::model.matrix(ff,newdata)
  out <- lava::estimate(model,robust=FALSE,f=function(p){ 1-exp(-exp(new.mat%*%matrix(p))*interval)})
  out <- cbind(newdata,summary(out)$coefmat)
  out
}

