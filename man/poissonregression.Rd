% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poissonregression.R
\name{poissonregression}
\alias{poissonregression}
\title{Poisson regression by aggregated data using glm}
\usage{
poissonregression(formula, data, aggvars, byvars)
}
\arguments{
\item{formula}{The formula as specified in glm.}

\item{data}{dataset.}

\item{aggvars}{vector of variables in data to be aggregated (summarized).}

\item{byvars}{vector of variables in data to aggregate by.}
}
\description{
Aggregates data and performs poisson regression using GLM.
}
