#' Part of the \code{medicinMacro}
#'
#' @title Calculates whether patients are exposed or not at a given time
#' @param drugs A named list of drugs. Each element of this list should be a list
#' with the following elements:
#' \describe{
#'   \item{atc}{A vector of ATC codes which should match the components of the drug exactly.}
#'   \item{maxdepot}{The maximum total dose that a single patient can possibly stack.}
#'   \item{period}{A vector of dates to limit the period in which to estimate the daily dose.}
#'   \item{prescriptionwindow}{Default is 2 prescriptions.}
#'   \item{doses}{A named list with the elements \code{value}, \code{min}, \code{max} and \code{def}.
#'   Here \code{value} is a vector of strengths of one unit (e.g. pill) of the drug.
#'   The vector should have one such strength for each of the different packages that occur in the data.
#'   \code{min} is a vector of the same length as \code{value} where each element is the assumed minimum total dose that a patient can consume on one day.
#'   \code{max} is a vector of the same length as \code{value} where each element is the assumed maximum total dose that a patient can consume on one day.
#'   \code{def} is a vector of the same length as \code{value} where each element is the assumed default dose that an average patient would consume on one day.
#'   See examples.}
#' }
#' @param drugdb data.table with (subset of) medical drugs registry
#' @param admdb data.table with (subset of) hospital admission registry. The data.table should be
#' prepared such that it contains only overnight hospital stay (i.e., pattype=2) and non-overlapping
#' hospital stay periods, i.e., as obtained with \code{getAdmLimits} (SAS-AKA: code-from-hell).
#' @param time.points Time points
#' @param window
#' Only used when the \code{type} = \code{"cross-sectional"}.
#' Specify how many days back from the given time point we should go to find purchases to estimate the daily dose.
#' @param method Default doses by default
#' @param stash Depot from prior prescriptions
#' @param cap.values Overwrite a last period with previous
#' @return Exposed or not at a given time
#' @author Anders Munch
mm1 <- function(drugs,
                drugdb,
                admdb,
                time.points,
                window,
                method="default.doses",
                stash=0,
                cap.values=TRUE,
                verbose=FALSE){
    time=last.potential.purch=eksd=pnr=exposed.at.time=drug.supply.days=h.days.since.potential.purch=hospitalized.at.time=NULL
    ind.periods <- copy(time.points)
    ind.periods[,":="(start=time-window, end=time)][,time:=NULL]
    exp.calc <- mm2(drugdb=drugdb,drugs=drugs,periods=ind.periods,admdb=admdb,method=method,stash=stash,cap.values=TRUE,pre.window=0,verbose=verbose)
    if(method=="number.of.days" & stash > 0)
        return(exp.calc) #' Should be NULL, warning given from mm2
    #' NB: _potential_ purchases, becuase a ptt's might not purchase anything in the time frame -- this gives a total.drug and drug.supply.days of 0
    last.dates <- exp.calc[,last.potential.purch:=max(eksd),by=pnr][last.potential.purch==eksd]
    cols.to.keep <- c("pnr","end","last.potential.purch",
                      "total.drug","average.drug","drug.strength.estimate",
                      "min.dose","max.dose","def.dose",
                      "estimated.daily.dose","drug.supply.days")    
    if(!is.null(admdb)){
        cols.to.keep <- c(cols.to.keep,"hospitalized.at.end","h.days")
        out <- last.dates[,cols.to.keep,with=FALSE]
        setnames(out, c("end","hospitalized.at.end","h.days"), c("time","hospitalized.at.time","h.days.since.potential.purch"))
        out[,exposed.at.time:=(time<=(last.potential.purch+drug.supply.days+h.days.since.potential.purch))]
        out[hospitalized.at.time==TRUE,exposed.at.time:=TRUE]
    }
    else{
        out <- last.dates[,cols.to.keep,with=FALSE]
        setnames(out, "end", "time")
        out[,exposed.at.time:=(time<=(last.potential.purch+drug.supply.days))]
    }
    return(out[])
}
