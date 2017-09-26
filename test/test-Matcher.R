library(testthat)
library(heaven)
library(data.table)
context("Matching")


test_that("Risk set matching",{
    n <- 55
    d=data.table(pnr=1:n,
                 case=rbinom(n,1,.23),
                 sex=rbinom(n,1,.4),
                 birthyear=round(runif(n,2000,2017)))
    # Very simple match without reuse - no dates to control for
    out <- riskSetMatch("ptid","case",c("byear","sex"),dat=d,2,NoIndex=TRUE)    
    ## expect_equal(out,)
})
