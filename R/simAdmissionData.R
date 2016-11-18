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
                             m = 5,
                             startDate = "1995-01-01", 
                             longformat = TRUE){
   startDate <- as.Date(startDate)
    out <- rbindlist(lapply(1:n,function(i){
        M = sample(1:m,size=1)
        ind <- startDate + runif(M,0,20*365.25)
        udd <- pmin(ind + runif(M,0,45), startDate + 20*365.25)
        dat.i = data.table(pnr=i,
                           inddto = ind,
                           uddto  = udd)
        dat.i
    }))
    setkey(out, pnr, inddto)
    setcolorder(out,c("pnr","inddto","uddto"))
    out
}



