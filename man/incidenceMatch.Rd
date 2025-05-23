% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incidenceMatch.R
\name{incidenceMatch}
\alias{incidenceMatch}
\title{incidenceMatch - Risk set matching for nested case-control designs}
\usage{
incidenceMatch(
  ptid,
  event,
  terms,
  data,
  n.controls,
  case.index = NULL,
  end.followup = NULL,
  date.terms = NULL,
  duration.terms = NULL,
  output.count.controls = TRUE,
  cores = 1,
  seed = 0,
  progressbar = TRUE
)
}
\arguments{
\item{ptid}{Personal ID variable defining participant}

\item{event}{Name of variable that defines cases. MUST be numeric 0/1 where 0 codes for never-case, and 1 for case.}

\item{terms}{Vector of variable names specifying the variables that should be matched on.
Make sure that
appropriate classification is in place for truly continuous variables, such as age. This is to
ensure a sufficient number of controls for each case. For example it may be
difficult to find controls for cases of very high and very low ages
and extreme ages should therefor further aggregated.}

\item{data}{The single dataset with all information - coerced to data.table
if data.frame}

\item{n.controls}{Number of controls for each case}

\item{case.index}{Name of the variable which contains the case index dates.
This can be a calendar date variable or a numeric variable, i.e., the time to outcome event
from a well defined baseline date. Missing values are interpreted as
no event at the end of followup.}

\item{end.followup}{Name of the variable which defines the date (as date or time)
from which a control can no longer be selected due to
\itemize{
\item{death: }{Nothing happens thereafter}
\item{other competing risks: }{Event after which we are not interested in the subject anymore. E.g., the date of an outcome event.}
\item{censoring: }{Event after which we do not observe anymore. E.g., emmigration, end of study period, end of registry, drop-out}
}
The end.followup must be larger or equal to 
the \code{case.index} date.}

\item{date.terms}{Unclear if useful in this context. But, see description
for \code{exposureMatch}.}

\item{duration.terms}{A list where each element defines a time duration
term with two elements:
\itemize{
\item{start}{Name of a variable which defines a date or time which 
defines the duration as the difference between this variable and the \code{case.index}.
}
\item{min}{Numeric value given in days when \code{case.index} is a date and in
the same time unit as \code{case.index} otherwise.
Cases and controls have either both a duration as least as long as \code{min}
or both a duration shorter than min.}
}
Useful to prepare to summarize the history of exposure for cases and controls in an equally long period
looking back in time from the \code{case.index}.}

\item{output.count.controls}{Logical. If \code{TRUE} add number of found controls to each case/control set.}

\item{cores}{number of cores to use in the calculation.}

\item{seed}{Random seed to make results reproducible}

\item{progressbar}{set to \code{FALSE} to avoid progressbar}
}
\value{
data.table with cases and controls. After matching, a the variable
"case.id" identifies sets which include 1 case and x matched controls.

Variables in the original dataset are preserved. The final dataset includes
all original cases but only the controls that were selected.
}
\description{
A case is a person who has an outcome event at the index date. A control is a person who does not yet have the event
at the index date of the case. See \code{exposureMatch} for situations where treatment start or onset of
a comorbidity define the case and the index date.

Risk set matching or incidence density sampling for nested case-control designs targets a a Cox regression model.
with time-dependent covariates.  In large-scale registry data the main purpose
for risk set matching, instead of standard Cox regression on all data, is to save computation time.
See Details and references.

Note that the parameter estimates of a conditional logistic regression analysis applied to the
output of \code{incidenceMatch}  are hazard ratios which should be interpreted in terms of a Cox regression
model.


To provide necessary speed for large samples the general technique used is
work with data.table and to create a series of match groups that have the fixed matching variables
identical (such as birthyear and gender).
}
\details{
The function performs exact matching and hence 
all matching variables must be factor variables or character.

It may appear tempting always to use multiple cores, but this comes at the 
cost of copying the data to the cores.

This function prepares the data for fitting a Cox regression model via \code{survival::clogit} or directly
via \code{survival::coxph} or equivalent routine. The regression parameters are hazard ratios. 
The matching variables are allowed to have a time-dependent non-proportional
effect on the hazard rate of the outcome very much in the same way as would be obtained without matching
by a strata statement to stratify the baseline hazard function. The original motivation for the nested case-control
design is when it is difficult, expensive or time-consuming to measure the exposure variables. 


The function matchReport may afterwards be used to provide simple summaries
of use of cases and controls
}
\examples{
require(data.table)
case <- c(rep(0,40),rep(1,15))
ptid <- paste0("P",1:55)
sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
case.Index <- c(seq(1,40,1),seq(5,47,3))
startDisease <- rep(10,55)
control.Index <- case.Index
diabetes <- seq(2,110,2)
heartdis <- seq(110,2,-2)
diabetes <- c(rep(1,55))
heartdis <- c(rep(100,55))
library(data.table)
dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,
control.Index,startDisease)
# Risk set matching
matchdat <- incidenceMatch(ptid="ptid",event="case",
            terms=c("byear","sex"),data=dat,n.controls=2,
            case.index="case.Index",
            end.followup="control.Index",seed=8)
matchdat
matchReport(matchdat)
# Same with 2 cores
library(parallel)
library(foreach)
\dontrun{
matchdat2 <- incidenceMatch("ptid","case",c("byear","sex"),data=dat,
   n.controls=2,case.index="case.Index",end.followup="control.Index"
  ,cores=2,seed=8)
matchdat2
all.equal(matchdat,matchdat2)
}

# Case control matching with requirement of minimum exposure time in each
# group
ew <- incidenceMatch(ptid="ptid",event="case",terms=c("byear","sex"),
      data=dat,n.controls=2,case.index="case.Index",
      end.followup="control.Index",cores=1,
      duration.terms=list(list(start="startDisease",min=15)))
ew

}
\references{
Bryan Langholz and Larry Goldstein. Risk set sampling in epidemiologic
   cohort studies. Statistical Science, pages 35--53, 1996.

Ornulf Borgan, Larry Goldstein, Bryan Langholz, et al. Methods for the
   analysis of sampled cohort data in the cox proportional hazards model. The
   Annals of Statistics, 23(5):1749--1778, 1995.

Vidal Essebag, Robert W Platt, Michal Abrahamowicz, and Louise Pilote.
   Comparison of nested case-control and survival analysis methodologies for
   analysis of time-dependent exposure. BMC medical research methodology, 5
   (1):5, 2005.
}
\seealso{
exposureMatch clogit matchReport Matchit
}
\author{
Christian Torp-Pedersen & Thomas Alexander Gerds
}
