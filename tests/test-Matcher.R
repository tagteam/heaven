library(testthat)
library(heaven)
library(data.table)
context("Matching")


test_that("Risk set matching",{
    library(data.table)
    n <- 355
    dat=data.table(pnr=1:n,
                 case=rbinom(n,1,.23),
                 sex=rbinom(n,1,.4),
                 birthyear=round(runif(n,2000,2017)),
                 case.date=rexp(n),
                 follow.up.date=rexp(n))
    dat$follow.up.date <- pmin(dat$follow.up.date,dat$case.date)
    dat$case.date[dat$case==0] <- NA
    # Very simple match without reuse - no dates to control for
    out <- incidenceMatch(ptid="pnr",
                          event="case",
                          terms=c("birthyear","sex"),
                          case.index="case.date",
                          end.followup="follow.up.date",
                          data=dat,
                          n.controls=2)
    ## expect_equal(out,)
})
