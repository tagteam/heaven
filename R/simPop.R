##' Simulate population with birthdate and gender
##'
##' Sample data for illustration, i.e., vignettes and example sections of the package.
##' @title Population simulation
##' @param n Number of subjects
##' @param min.age minimal age 
##' @param max.age maximal age
##' @param sex gender distribution: value between 0 and 1
##' @param mortality risk of death: value between 0 and 1 
##' @return data.table with columns: pnr, sex, birthdate, status, doddate
##' where doddate is the date of death for subjects with status = 1 and
##' the current date for subjects with status = 0.
##' @seealso simPrescriptionData, simAdmissionData
##' @examples
##' set.seed(7)
##' simPop(7)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
simPop <- function(n,min.age=20,max.age=90,sex=.5,mortality=.05){
    doddate=status=birthdate=NULL
    pop <- data.table::data.table(pnr=1:n,
                                  sex=factor(rbinom(n,1,sex),levels=0:1,labels=c("female","male")),
                                  birthdate=Sys.Date()-365.25*runif(n,min.age,max.age),status=rbinom(n,1,mortality))
    pop[,doddate:=Sys.Date()]
    pop[status==1,doddate:=runif(sum(status==1),
                                 max(birthdate[status==1],as.Date("1995-01-01")),
                                 Sys.Date())]
    data.table::rbindlist(list(NULL,pop))
}
