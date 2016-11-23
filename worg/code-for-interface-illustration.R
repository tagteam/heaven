#devtools::install_github("tagteam/heaven", force = TRUE, Rcpp = TRUE)

library(heaven)
library(Publish)

d <- dpp()

set.seed(8)
drugdata <- simPrescriptionData(10, startDate = "2006-01-01")
drugdb(d) <- drugdata
org(head(d$drugdb))

admdata <- rbind(simAdmissionData(10, startDate = "2006-01-01"))
admdb(d) <- admdata
org(head(d$admdb))

drug(d, ex1) <- atc("A12B")
drug(d, ex2) <- atc("A07")
drug(d, ex1) <- atc(c("A12B"))
drug(d, ex2) <- atc(c("A07"))
drug(d, ex1) <- pack(c(750, 75), 
                     min = c(250, 25),
                     max = c(1000, 100), 
                     def = c(750, 100))
drug(d, ex2) <- pack(c(200, 400, 500), 
                     min = c(100, 100, 250),
                     max = c(400, 500, 1000), 
                     def = c(300, 200, 500))

plot(d, id = 4, drug = "ex1")
N(d) <- 3
maxrep(d) <- 50

ex <- process(d, out=FALSE, id = 3) 

(ex1out <- ex$ex1)
(ex2out <- ex$ex2)
org(head(ex1out[, names(ex1out) %in% c("id", "X", "B", "E", "R")]))

plot(ex, id=3, drug="ex2")

ex1 <- process(d, id=2, out=FALSE) 
ex1




is(ex1, "dppout")
ex2 <- process(d, id=2, out=TRUE) 
ex2
attr(ex2, "class")
plot(ex2)
plot(ex1)
attr(ex1, "type")
inherits(ex2, "class")
