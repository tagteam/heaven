library(testthat)
library(heaven)
library(data.table)
context("medicin macro")

# Test to see if value for one individual is the same when calculated manually.
# Also check wether values for subset of a dataset are the same as when the whole dataset is processed.
# (NB: however, does this need to always be true?)

test_that("gives correct values for one subject",{
    # Create some data
    n <- 1
    set.seed(9)
    drugdata <- simPrescriptionData(n, startDate = "2006-01-01")[1,]
    # Setup the dpp object
    d <- dpp()
    drugdb(d) <- drugdata
    drug(d, "Treatment2") <- atc("A07")
    drug(d, "Treatment2") <- pack(c(200, 400, 500), 
                                  min = c(100, 100, 250),
                                  max = c(400, 500, 1000), 
                                  def = c(300, 200, 500))
    pwindow(d) <- 2
    maxdepot(d) <- maxdepot <- 4000
    period(d) <- as.Date(c("2005-01-01", "2017-12-31"))
    # Manual calc.
    # est. daily dose:
    ind <- which(d$drugs$Treatment2$doses$value==drugdata$strnum)
    defdose <- d$drugs$Treatment2$doses$def[ind]
    manual.est <- round(drugdata$strnum/defdose*drugdata$packsize)-1
    # Estimate from the process function:
    out <- process(d)
    auto.est <- as.numeric(out$processed$Treatment2$E-out$processed$Treatment2$B)
    expect_equal(manual.est,auto.est)
    # Try to process a full set of 5 individual and subset of 1 individual.     
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
    out.full <- process(d)
    # Try to do the same on only one individual
    n <- 1
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
    out.one <- process(d)
    # Check same result on a subset
    expect_equal(out.one$processed$Treatment1,
                 out.full$processed$Treatment1[1:dim(out.one$processed$Treatment1)[1],])
    expect_equal(out.one$processed$Treatment2,
                 out.full$processed$Treatment2[1:dim(out.one$processed$Treatment2)[1],])
})
