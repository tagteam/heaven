rm(list = ls())
library(data.table)
library(testthat)
source("R/patientProfiles.R")



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

test_that("Output is correct", {
  n <- 1e1
  set.seed(13)
  x <- data.frame(
    v2 = sample(c("yes", "no"), n, replace = TRUE),
    age = sample(20:22, size = n, replace = TRUE),
    v3 = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.2, 0.8))
  )
  setDT(x)
  x[,v2 := as.character(v2)]
  out <- patientProfile(dt = x, primary.cov = "age", "v2", "v3")
  
  
  true_out <- data.table(join.id = c("21:no:no", "21:yes:no",
                                     "22:no:no", "22:yes:no"),
                         age = c(21,21,22,22),
                         v2 = as.factor(c("no", "yes", "no", "yes")),
                         v3 = as.factor(c(rep("no", 4))),
                         count = c(4,2,1,3),
                         N.in.primary.cov = c(6,6,4,4),
                         prop.by.primary.cov = c(2/3, 1/3, 0.25, 0.75))
  setkey(true_out, "age")
  all.equal(true_out, out)
  expect_equal(out, true_out)
})

