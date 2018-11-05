library(testthat)
library(heaven)
library(data.table)
context("medicin macro")


test_that("process/innerprocess runs",{
    # Create some data
    n <- 5
    set.seed(9)
    drugdata <- simPrescriptionData(n, startDate = "2006-01-01")
    admdata <- simAdmissionData(n, startDate="2006-01-01")
    # Setup the dpp object
    d <- dpp()
    drugdb(d) <- drugdata
    admdb(d) <- admdata
    drug(d, "Treatment1") <- atc("A12B")
    drug(d, "Treatment2") <- atc("A07")
    drug(d, "Treatment1") <- pack(c(750, 75), 
                                  min = c(250, 25),
                                  max = c(1000, 100), 
                                  def = c(750, 100))
    drug(d, "Treatment2") <- pack(c(200, 400, 500), 
                                  min = c(100, 100, 250),
                                  max = c(400, 500, 1000), 
                                  def = c(300, 200, 500))
    pwindow(d) <- 2
    maxdepot(d) <- maxdepot <- 4000
    period(d) <- as.Date(c("2005-01-01", "2017-12-31"))
    # Just check that no error happens -- now or before
    out <- process(d)
    out <- process(d, collapse = FALSE)
})


