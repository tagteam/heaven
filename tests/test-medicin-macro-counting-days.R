## Testing basic estimates from medicin-macro

library(testthat)
library(heaven)
library(data.table)
context("medicin-macro: exposure days estimates")

test_that("One purchase", {
    # Simulate data
    lmdb0 <- data.table(pnr=8,
                        eksd=as.Date("2005-01-25"),
                        apk=1,
                        atc="C10AA01",
                        strnum=10,
                        packsize=0.0999998)
    simva0 <- list(atc="C10AA01",maxdepot=8000,
                   period=as.Date(c("2004-01-01","2015-12-31")),
                   prescriptionwindow=2, ## consider 2 previous purchases and current
                   doses=list(value=c(10,20,30),min=c(5,10,15),max=c(20,40,60),def=c(10,20,30)))
    x <- medicinMacro(drugs=list(simva=simva0),drugdb=lmdb0,admdb=NULL)
    expect_equal(as.numeric(x$simva$exposure.days), 0)
})

test_that("Two purchases -- with and without overlap", {
    # Simulate data
    lmdb0 <- data.table(pnr=c(8,8),
                        eksd=as.Date(c("2005-01-16","2005-01-19")),
                        apk=c(1,1),
                        atc=c("C10AA01", "C10AA01"),
                        strnum=c(10,10),
                        packsize=c(5,5))
    simva0 <- list(atc="C10AA01",maxdepot=8000,
                   period=as.Date(c("2004-01-01","2015-12-31")),
                   prescriptionwindow=2, ## consider 2 previous purchases and current
                   doses=list(value=10,min=10,max=10,def=10))
    # With overlap
    x <- medicinMacro(drugs=list(simva=simva0),drugdb=lmdb0,admdb=NULL)
    expect_equal(as.numeric(x$simva$exposure.days), 10)
    # Without overlap
    lmdb0$eksd[2] <- as.Date("2005-01-23")
    x <- medicinMacro(drugs=list(simva=simva0),drugdb=lmdb0,admdb=NULL)
    expect_equal(as.numeric(x$simva$exposure.days), c(5,2,5))
})

test_that("Two purchases with different doses", {
    # Simulate data
    lmdb1 <- data.table(pnr=c(8,8),
                        eksd=as.Date(c("2005-01-16","2005-01-18")),
                        apk=c(1,1),
                        atc=c("C10AA01", "C10AA01"),
                        strnum=c(10,30),
                        packsize=c(5,5))
    simva1 <- list(atc="C10AA01",maxdepot=8000,
                   period=as.Date(c("2004-01-01","2015-12-31")),
                   prescriptionwindow=2, ## consider 2 previous purchases and current
                   doses=list(value=c(10,30),min=c(10,15),max=c(10,15),def=c(10,15)))
    # check results
    x <- medicinMacro(drugs=list(simva1=simva1),drugdb=lmdb1,admdb=NULL)
    expect_equal(as.numeric(x$simva$exposure.days), c(2,12))  
})
