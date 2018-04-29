##' Simulate admission data
##' 
##' Simulate admission data alike the Danish medical registry
##' @title Admission data simulation function
##' @aliases simLPR simAdmissionData
##' @param n Number of patients
##' @param m Maximal number of admission dates per patient
##' @param diagnoses List of diagnoses. Defaults to all possible ICD10 codes: http://www.icd10data.com/ICD10CM/Codes
##' @param startDate starting date
##' @author Helene Charlotte Rytgaard \email{hely@@sund.ku.dk}, Thomas Alexander Gerds \email{tag@@biostat.ku.dk}
##' 
##' @examples
##' ## a single subject
##' set.seed(1)
##' simAdmissionData(1)
##' ## 7 subjects
##' set.seed(28)
##' simAdmissionData(7)
##' 
##' @export
simAdmissionData <- function(n,
                             m = 5,
                             diagnoses=paste0(toupper(letters),rep(0:99,length(letters))),
                             startDate = "1995-01-01"){
    startDate <- as.Date(startDate)
    out <- data.table::rbindlist(lapply(1:n,function(i){
        M = sample(1:m,size=1)
        ind <- startDate + runif(M,0,20*365.25)
        udd <- pmin(ind + runif(M,0,45), startDate + 20*365.25)
        dat.i = data.table::data.table(pnr=i,
                                       inddto = ind,
                                       uddto  = udd,
                                       diag = sample(diagnoses,size=M),
                                       pattype=sample(1:4,size=M,replace=TRUE))
        dat.i
    }))
    data.table::setkey(out, pnr, inddto)
    data.table::setcolorder(out,c("pnr","inddto","uddto","diag","pattype"))
    out
}
#' @export
simLPR <- simAdmissionData


