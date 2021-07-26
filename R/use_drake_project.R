#'@Title Sets up your directory for a drake project
#'
#'@description Helper function that will create some scafolding to help you
#'  easily start a drake project
#'@return Modifies your directiory.
#'@author Matthew Phelps
#'@export
use_drake_project <- function() {
  usethis::use_directory("R")
  usethis::use_directory("output")
  usethis::use_directory("input")
  usethis::use_template("packages.R", package = "heaven")
  usethis::use_template("_drake.R", package = "heaven")
  usethis::use_template("run_local.R", package = "heaven")
  usethis::use_template("run_r_make.R", package = "heaven")
  usethis::use_template("plan.R", save_as = "/R/plan.R", package = "heaven")
  
}