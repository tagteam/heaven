#devtools::install_github("tagteam/heaven", force = TRUE, Rcpp = TRUE)
library(heaven)
library(Publish)

T <- as.Date(c(rep("2012-05-08", 3), "2012-08-11", rep("2013-03-15", 2), "2013-05-05",
               "2013-05-25", rep("2013-10-01", 2), "2013-12-28"))
exdata <- data.frame(pnr = rep(1, length(T)), 
                     eksd = T,
                     atc = rep("A07", length(T)), 
                     packsize = c(1, 1, 1, 1, 1, 2, 1, 3, 2, 1, 1),
                     apk = c(10, 15, 10, 15, 10, 10, 10, 15, 10, 10, 10), 
                     strnum = c(50, 50, 80, 50, 50, 50, 80, 80, 50, 80, 80))
d <- dpp()

set.seed(8)

drugdb(d) <- exdata
d$drugdb

drug(d, exdrug) <- atc(c("A07", "A12B"))
drug(d, exdrug) <- pack(c(50, 80), 
                        min = c(25, 20),
                        max = c(100, 100), 
                        def = c(50, 60))
d
set.seed(20)

#drugdb(d) <- simPrescriptionData(1, m = 10, startDate = as.Date("2006-01-01"))
admdb <- data.frame(id = rep(1, 2),
                    inddto = as.Date(c("2012-06-11", "2013-01-03")),
                    uddto  = as.Date(c("2012-06-28", "2013-01-18")))
admdb(d, id = id) <- admdb
d$admdb
d$drugdb
d
plot(d)
N(d) <- 3
ex <- process(d, out=FALSE)
ex
exdata
plotoutput(ex, which=1)
plotoutput(ex)
org(ex$exdrug[, names(ex$exdrug) %in% c("B", "M")])


ex1 <- process(d)
ex1
