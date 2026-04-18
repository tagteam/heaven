library(testthat)
library(heaven)
library(data.table)
context("Matching")

test_that("Risk set matching smoke test", {
    set.seed(1)
    n <- 60
    dat <- data.table(
        pnr = 1:n,
        case = rbinom(n, 1, .15),
        sex = rbinom(n, 1, .4),
        birthyear = round(runif(n, 2000, 2017)),
        case.date = rexp(n),
        follow.up.date = rexp(n)
    )
    dat[, follow.up.date := pmin(follow.up.date, case.date)]
    dat[case == 0, case.date := NA_real_]
    out <- incidenceMatch(
        ptid = "pnr",
        event = "case",
        terms = c("birthyear", "sex"),
        case.index = "case.date",
        end.followup = "follow.up.date",
        data = dat,
        n.controls = 1
    )
    expect_true(is.data.frame(out) || is.data.table(out))
})
