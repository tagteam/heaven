##' Simulate admission data
##' 
##' Simulate admission data alike the Danish medical registry
##' @title Admission data simulation function
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
                             diagnoses,
                             startDate = "1995-01-01"){
    if (missing(diagnoses)) {
        diagnoses <- c("DN162D",
                      "DV1180",
                      "DN982",
                      "DT698",
                      "DJ343",
                      "DP389C",
                      "DD484",
                      "DB741",
                      "DO721A",
                      "DQ728D",
                      "DK254E",
                      "DT635",
                      "DB601A",
                      "DD239E",
                      "DQ794A",
                      "DO010",
                      "DL923B",
                      "DD223Z",
                      "DF0122",
                      "DZ237",
                      "DE519",
                      "DG461",
                      "DO472",
                      "DK265D",
                      "DN330",
                      "DM92",
                      "DUA19")
    }
    pnr=inddto=recnum=NULL
    startDate <- as.Date(startDate)
    ## out <- foreach::foreach(i=1:n,.combine="rbind") %dopar% {
    out <- NULL
    ## out <- foreach::foreach(i=1:n,.combine="rbind") %dopar% {
    for(i in 1:n){
        ## out <- data.table::rbindlist(lapply(1:n,function(i){
        M = sample(1:m,size=1)
        ind <- startDate + runif(M,0,20*365.25)
        udd <- pmin(ind + runif(M,0,45), startDate + 20*365.25)
        pattype <- sample(1:3,size=1,replace=TRUE)
        indexdate <- startDate+runif(M,0,20*365.25)
        dat.i = data.table::data.table(pnr=i,inddto = ind,uddto  = udd,diag = sample(diagnoses,size=M,replace = TRUE),indexdate,pattype)
        out <- rbindlist(list(dat.i,out))
    }
    data.table::setkey(out, inddto)
    out[,recnum:=1:nrow(out)]
    data.table::setkey(out, pnr, inddto)
    data.table::setcolorder(out,c("pnr","recnum","inddto","uddto","indexdate","diag","pattype"))
    out[]
}



