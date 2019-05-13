library(data.table)
library(testthat)
library(heaven)
context("patient profiles")

test_that("output is data.table", {
  n <- 1e1
  set.seed(13)
  x <- data.frame(
    age = sample(20:35, size = n, replace = TRUE),
    v2 = sample(c("yes", "no"), n, replace = TRUE),
    v3 = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.2, 0.8))
  )
  setDT(x)
  out <- patientProfile(dt = x, primary.cov = "age", "v2", "v3")
  expect_that(out, is_a("data.table"))
})

