##' Simulate prescription data
##' 
##' Simulate prescription data alike the Danish medical registry
##' @title Prescription data simulation function
##' @param n Number of patients
##' @param m Maximal number of prescription dates per patient
##' @param packages Named list of association lists. The names of the list are ATC codes.
##' The association list consists of drug strength and package size (number of pills). See details.
##' @param max.packages
##' @author Helene Charlotte Rytgaard \email{hely@@sund.ku.dk}, Thomas Alexander Gerds \email{tag@@biostat.ku.dk} 
##' @details 
##' 
##' http://www.medicinpriser.dk/
##'
##' A07: strength 200, number of pills 30
##'      strength 400, number of pills 100
##'      etc
##'
##' A12: strength 750, number of pills 100
##'      strength 750, number of pills 250
##'      etc
##' 
##' Association list to be used as package argument 
##'
##' \code{list("A07"=list(c(200,30),c(400,100),c(400,300),c(500,60)),
##'       "A12B"=list(c(750,100),c(750,250),c(75,500)))}
##' 
##' @examples
##' 
##' set.seed(1)
##' simPrescriptionData(1)
##' set.seed(2)
##' simPrescriptionData(2)
##' 
##' @export
simPrescriptionData <- function(n,
                                m=10,
                                packages=list("A07"=list(c(200,30),c(400,100),c(400,300),c(500,60)),
                                              "A12B"=list(c(750,100),c(750,250),c(75,500))),
                                max.packages=3,
                                startDate = "1995-01-01"){
    startDate <- as.Date(startDate)
    out <- rbindlist(lapply(1:n,function(i){
        pat.i <- rbindlist(lapply(1:length(packages),function(p){
            pack <- unlist(packages[p],recursive=FALSE)
            M=sample(1:m,size=1) ## number of prescription dates
            data.table(eksd = startDate + rbinom(M, 1, 0.95)*runif(M,0,5*365.25),
                       atc = names(packages)[p],
                       packsize = sample(sapply(pack,"[",2),size=M,replace=TRUE),
                       apk=sample(1:max.packages,size=M,replace=TRUE),
                       strnum = sample(sapply(pack,"[",1),size=M,replace=TRUE))
        }))
        pat.i[,pnr:=i]
    }))
    setkey(out,pnr,atc,eksd)
    setcolorder(out,c("pnr","atc","eksd","strnum","packsize","apk"))
    out
}


