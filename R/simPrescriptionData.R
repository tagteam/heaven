##' Simulate prescription data
##' 
##' Simulate prescription data alike the Danish medical registry
##' @title Prescription data simulation function
##' @param n Number of patients
##' @param max.prescriptions Maximal number of prescription dates per patient
##' @param packages Named list of association lists. The names of the
##'     list are ATC codes, the value associates strengths of one unit and package sizes.
##' @param max.packages Integer. Upper bound for number of packages purchased on a single date
##' @param startDate start date
##' @author Helene Charlotte Rytgaard \email{hely@@sund.ku.dk}, Thomas
##'     Alexander Gerds \email{tag@@biostat.ku.dk}
##' @details
##'
##' http://medinfo.dk/sks/brows.php
##' http://www.medicinpriser.dk/
##' http://pro.medicin.dk/
##' https://www.whocc.no/atc_ddd_index/
##'
##' A07: strength 200, number of pills 30
##'      strength 400, number of pills 100
##'      etc
##'
##' A12: strength 750, number of pills 100
##'      strength 750, number of pills 250
##'      etc
##' 
##' # The association list which is used as the default value
##' # for the \code{package} argument:
##'
##' \code{list("A07"=list(c(200,30),c(400,100),c(400,300),c(500,60)),
##'       "A12B"=list(c(750,100),c(750,250),c(75,500)))}
##' 
##' @examples
##' ## a single subject
##' set.seed(1)
##' simPrescriptionData(1)
##' ## 7 subjects
##' set.seed(29)
##' simPrescriptionData(7)
##' 
##' @export
simPrescriptionData <- function(n,
                                max.prescriptions=37,
                                packages=list(list(c(200,30),c(400,100),c(400,300),c(500,60)),
                                              list(c(750,100),c(750,250),c(75,500))),
                                max.packages=3,
                                startDate = "1995-01-01"){
    pnr=eksd=NULL
    startDate <- as.Date(startDate)
    if (is.null(names(packages))) {
        atc <- c("R06AC01",
                 "N01BB51",
                 "D08AX01",
                 "N02AA03",
                 "C09BA02",
                 "B02BC",
                 "L01AD02",
                 "C08CA08",
                 "L01AA02",
                 "J07BB",
                 "R05CB15",
                 "C09DA04",
                 "L04AA32",
                 "J05AE03",
                 "N04BD03",
                 "M03AX01",
                 "A12AX",
                 "D06BB12",
                 "R03DA03",
                 "A06AB06",
                 "L03AA10",
                 "N07BB05",
                 "A09AA02",
                 "G02AD04",
                 "H05BA01",
                 "C03CB02",
                 "C01EB17")
    }else{
        atc <- names(packages)
    }
    out <- data.table::rbindlist(lapply(1:n,function(i){
        pat.i <- data.table::rbindlist(lapply(1:length(packages),function(p){
            pack <- unlist(packages[p],recursive=FALSE)
            M=sample(1:max.prescriptions,size=1) ## number of prescription dates
            out <- data.table::data.table(eksd = startDate + rbinom(M, 1, 0.95)*runif(M,0,5*365.25),
                                          atc = sample(atc,size=M,replace=TRUE),
                                          packsize = sample(sapply(pack,"[",2),size=M,replace=TRUE),
                                          apk=sample(1:max.packages,size=M,replace=TRUE),
                                          strnum = sample(sapply(pack,"[",1),size=M,replace=TRUE))
            out
        }))
        pat.i[,pnr:=i]
    }))
    data.table::setkey(out,pnr,atc,eksd)
    data.table::setcolorder(out,c("pnr","atc","eksd","strnum","packsize","apk"))
    out
}
