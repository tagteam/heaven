#devtools::install_github("tagteam/heaven", force = TRUE, Rcpp = TRUE)

library(heaven)
library(Publish)
set.seed(8)
drugdata <- simPrescriptionData(10)
admdata <- simAdmissionData(10)
d <- dpp()
drugdb(d) <- drugdata
admdb(d) <- admdata
drug(d, "drug1") <- atc("A12B")
drug(d, "drug1") <- pack(c(750, 75), 
                            min = c(250, 25), 
                            max = c(1000, 100), 
                            def = c(750, 100))
drug(d, "drug2") <- atc("A07")
drug(d, "drug2") <- pack(c(200, 400, 500), 
                     min = c(100, 100, 250),
                     max = c(400, 500, 1000), 
                     def = c(300, 200, 500))
pwindow(d) <- 3
out <- process(d, keep_data = TRUE)

plot(out, id = 5, trace = TRUE)



