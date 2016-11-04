##' Simulate admission data
##' 
##' Simulate admission data alike the Danish medical registry
##' @title Admission data simulation function 
##' @param n Number of patients
##' @param m Maximal number of admission dates per patient
##' @author Helene Charlotte Rytgaard \email{hely@@sund.ku.dk}
##' @details 
##' 
##' @examples
##' 
##' set.seed(1)
##' simAdmissionData(1)
##' set.seed(2)
##' simAdmissionData(2)
##' 
##' @export
simAdmissionData <- function(n,
                             m=5,
                             longformat=TRUE){
    startDate = as.Date("1995-01-01")
    out <- rbindlist(lapply(1:n,function(i){
        M = sample(0:m,size=1)
        dates    = matrix(sort(startDate + runif(M*2,0,10*365.25)), 2, M)
        dat.i = data.table(inddto = as.Date(dates[1, ], origin = "1970-01-01"),
                           uddto  = as.Date(dates[2, ], origin = "1970-01-01"))
        dat.i[, pnr:=i]
    }))
    setkey(out, pnr, inddto)
    out
}



