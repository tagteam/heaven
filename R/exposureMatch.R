### exposureMatch.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jun 26 2019 (08:57) 
## Version: 
## Last-Updated: Jul  3 2019 (15:14) 
##           By: Thomas Alexander Gerds
##     Update #: 45
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#' @title Exact matching on time-dependent exposure 
#'
#' @description
#'
#' A case is a person who initiates treatment at index date. A control is a person who has not yet initiated treatment
#' at the index date of the case. The index date of the case can also be the onset of a disease (or other exposure),
#' in which case controls are persons who do not yet have the disease (are not yet exposed). See \code{incidenceMatch} for
#' situations where a case has a time to event outcome at the index date and controls do not have the outcome at the case index date. 
#'
#' Within a cohort or registry study, a number of controls
#' are matched to each case on demographics, comorbidity, concomitant medication and other pre-treatment person characteristics
#' measured at the index date of the case.
#'
#'
#' Matching may be useful in non-randomized studies when the aim is to estimate a 
#'\itemize{
#' \item{causal parameter: }{The average effect of treatment in a hypothetical study which
#' randomizes treatment allocation. E.g., the difference or ratio of the absolute risk of
#' developing an outcome within the first 6 months after the index date.}
#' \item{regression parameter: }{The conditional effect of treatment (exposure)
#' on an outcome given pre-treatment (pre-exposure) covariates. E.g., a hazard ratio in
#' a Cox regression model, or an odds ratio in a logistic regression model. In particular, when
#' it is difficult or expensive or time-consuming to measure pre-exposure variables or to remove
#' model dependency. See Details and references.}
#' }
#' @author Christian Torp-Pedersen & Thomas Alexander Gerds
#' @param ptid  Personal ID variable defining participant
#' @param event Name of variable that defines cases. MUST be numeric 0/1 where 0 codes for never-case, and 1 for case.
#' @param terms Vector of variable names specifying the variables that should be matched on.
#' Make sure that
#' appropriate classification is in place for truly continuous variables, such as age. This is to
#' ensure a sufficient number of controls for each case. For example it may be
#' difficult to find controls for cases of very high and very low ages
#' and extreme ages should therefor further aggregated. 
#' @param data The single dataset with all information - coerced to data.table
#' if data.frame
#' @param n.controls Number of controls for each case
#' @param case.index Name of the variable which contains the case index dates.
#' This can be a calendar date variable or a numeric variable, i.e., the time to outcome event
#' from a well defined baseline date. Missing values are interpreted as
#' no event at the end of followup.
#' @param end.followup Name of the variable which defines the date (as date or time)
#' from which a control can no longer be selected as a control due in general to.
#' \itemize{
#' \item{death: }{Nothing happens thereafter}
#' \item{exposure of interest: }{a control can be selected among those that later develop condition of interest, but not after the date of exposure}
#' \item{other competing risks: }{Event after which we are not interested in the subject anymore. E.g., the date of an outcome event.}
#' \item{censoring: }{Event after which we do not observe anymore. E.g., emmigration, end of study period, end of registry, drop-out}
#' }
#' The end.followup must be larger or equal to 
#' the \code{case.index} date. 
#' @param date.terms A vector of variable names (character) in \code{data}
#' specifying dates of conditions such as comorbidities. Controls are considered as
#' matched only when the \code{date.terms} of the control are on the same chronological
#' side of case index as for the case.
#' Missing values are interpreted
#' as absence of comorbidity at \code{end.followup}.
#' Many datasets have comorbidities as time dependent variables. Matching on
#' these requires that the comorbidity date is not (yet) reached for a corresponding
#' variables for cases if the case does not have the comorbidity and
#' similarly that the date has been reached when the case does have that co-morbidity. 
#' @param duration.terms A list where each element defines a time duration
#' term with two elements:
#' \itemize{
#' \item{start}{Name of a variable which defines a date or time which 
#' defines the duration as the difference between this variable and the \code{case.index}.
#' }
#' \item{min}{Numeric value given in days when \code{case.index} is a date and in
#' the same time unit as \code{case.index} otherwise.
#' Cases and controls have either both a duration as least as long as \code{min}
#' or both a duration shorter than min.}
#' }
#' Useful to prepare to summarize the history of exposure for cases and controls in an equally long period looking back in time from the \code{case.index}.
#' @param output.count.controls Logical. If \code{TRUE} add number of found controls to each case/control set.
#' @param cores number of cores to use in the calculation.
#' @param seed Random seed to make results reproducible
#' @param progressbar set to \code{FALSE} to avoid progressbar
#' @details
#'
#' The function performs exact matching and hence 
#' all matching variables must be factor variables or character.
#' 
#' In some cases there is a necessity to account for delayed entry. An example
#' could be to ensure that people are not selected as controls prior to
#' immigration to the country.  This can be handled with "date.terms". Cases and
#' controls can as an example be provided with a date.term that is maximum of 
#' the date of birth and the date of immigration.
#' 
#' In some cases it makes sense to think about the study population of a hypothetical study
#' with randomized treatment allocation. Then, the aim of the function is similar to
#' that of propensity score analysis and exact matching may be preferred as it does not
#' dependent on a propensity score model (see King & Nielsen 2019).
#'
#' It may appear tempting always to use multiple cores, but this comes at the 
#' cost of copying the data to the cores. 
#'
#' The function matchReport may afterwards be used to provide simple summaries
#' of use of cases and controls
#' @return data.table with cases and controls. After matching, a the variable
#' "case.id" identifies sets which include 1 case and x matched controls.
#'
#' Variables in the original dataset are preserved. The final dataset includes
#' all original cases but only the controls that were selected.
#' @references
#'
#' Paul R Rosenbaum. Risk set matching. Design of observational studies, Springer, 2010.
#'
#' Yunfei Paul Li, Kathleen J Propert, and Paul R Rosenbaum. Balanced risk set
#' matching. Journal of the American Statistical Association, 96(455):870--
#'  882, 2001.
#'
#' Mohammad A Mansournia, Miguel A Herna'n, and Sander Greenland. Matched
#'    designs and causal diagrams. International journal of epidemiology, 42(3):
#'    860--869, 2013.
#' 
#' King, G., & Nielsen, R.
#' Why Propensity Scores Should Not Be Used for Matching.
#' Political Analysis, 1-20. doi:10.1017/pan.2019.11
#' 
#' @seealso riskSetMatch matchReport Matchit
#' @examples
#' require(data.table)
#' case <- c(rep(0,40),rep(1,15))
#' ptid <- paste0("P",1:55)
#' sex <- c(rep("fem",20),rep("mal",20),rep("fem",8),rep("mal",7))
#' byear <- c(rep(c(2020,2030),20),rep(2020,7),rep(2030,8))
#' case.Index <- c(seq(1,40,1),seq(5,47,3))
#' startDisease <- rep(10,55)
#' control.Index <- case.Index
#' diabetes <- seq(2,110,2)
#' heartdis <- seq(110,2,-2)
#' diabetes <- c(rep(1,55))
#' heartdis <- c(rep(100,55))
#' library(data.table)
#' dat <- data.table(case,ptid,sex,byear,diabetes,heartdis,case.Index,
#' control.Index,startDisease)
#' # Exposure density matching
#' matchdat <- exposureMatch("ptid","case",c("byear","sex"),data=dat,n.controls=2,
#' case.index="case.Index",end.followup="control.Index")
#' matchdat
#' # Same with 2 cores
#' \dontrun{
#' library(parallel)
#' library(foreach)
#' matchdat2 <- exposureMatch("ptid","case",c("byear","sex"),data=dat,2,case.index=
#'   "case.Index",end.followup="control.Index"
#'   ,cores=2)
#' matchdat2
#' all.equal(matchdat,matchdat2)
#' }
#' #Time dependent matching. In addition to fixed matching parameters there are
#' #two other sets of dates where it is required that if a case has that condi-
#' #tion prior to index, then controls also need to have the condition prior to
#' #the case index to be eligible - and if the control does not have the condi-
#' #tion prior to index then the same is required for the control.
#' matchdat <- exposureMatch("ptid","case",c("byear","sex"),data=dat,n.controls=2,
#'   case.index="case.Index",end.followup="control.Index",cores=1,
#'   date.terms=c("diabetes","heartdis"))
#' matchdat
#'
#' @include riskSetMatch.R
#' @export
exposureMatch <- riskSetMatch

######################################################################
### exposureMatch.R ends here
