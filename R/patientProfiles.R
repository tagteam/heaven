#' Profile of common covariate combinations among patients
#'
#' @param dt Data.table object where each row represents a patient and each row
#'   represents a covariates (or risk factor)
#' @param primary.cov An integer, character, or factor vector. The primary
#'   covariate to group by and calculate proportions for (usually age)
#' @param ... Additional covariates to group by - e.g. gender, status of
#'   previous exposures/events, etc. Accepts vectors of integers, characters or
#'   factors
#'
#' @details The function is designed to show how many patients exists in the
#'   dataset that have each combination of risk factors.
#'
#'   Covariate combinations will that conatin 0 patients will return a 0 value
#'   for that covariate combination. However, this assumes that all possible
#'   outcomes for each variable are found in at least one patient in the
#'   dataset. For example, using "age" as a covariate, if the dataset ranges
#'   from 20 to 50 years olds, it is assumed that at least one patient from
#'   every interger age is present i.e. 20, 21, 22....50. If there are no
#'   patients aged 22, for example, then no covariate combinations with patients
#'   aged 22 will be created.
#' @return Returns a data.table object where each row is a covariate
#'   combination. \code{N.in.primary.cov} records the number of patients in the
#'   data set with the specified primary covariate. E.g. using the "age" example
#'   from the "details" section, this column would record how many patients are
#'   20, 21, 22....50. \code{prop.by.primary.cov} records the proportion of patients with the
#'   same primary covariate (\code{primary.cov}) with that parameter
#'   combination.
#'
#'
#'
#'
#' @examples
#' library(data.table)
#'   n <- 1e2
#'   set.seed(13)
#'   x <- data.frame(
#'     age = sample(20:35, size = n, replace = TRUE),
#'     v2 = sample(c("yes", "no"), n, replace = TRUE),
#'     v3 = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.2, 0.8))
#'   )
#'   setDT(x)
#'   out <- patientProfile(dt = x, primary.cov = "age", "v2", "v3")
#'
#' @author Matthew Phelps \email{mphelps@@hjerteforeningen.dk}
#' @export
#' 
patientProfile <- function(dt, primary.cov, ...) {
  # Function returns the proportion of patients with each combination of
  # covaraites who have the same age. i.e. the proportion of 55 year olds with
  # each covariate combination.
  # browser()
  # Error catching:
  if (!("data.table" %in% (.packages())))
    stop("Please load data.table package")
  if (!is.data.table(dt))
    stop("Input data (dt argument) needs to be in a data.table format")
  arguments <- c(primary.cov, list(...))
  if (!all(sapply(arguments, is.character)))
    stop("Column names need to be in character format - i.e. put them in quotation marks")
  
  if (length(arguments) != ncol(dt))
    stop(
      "Mismatch in number of columns specified.
      You either entered too many or too few column arguments"
    )
  
  if (any(sapply(arguments, is.numeric)))
    stop(
      "Columns of class numeric are not allowed, please convert all columns to integer, character, or factor"
    )
  
  # Allows us to generalize the primary covariate to group by
  setnames(dt, arguments[[1]], "primary.cov")
  # Move primary.cov to front of dt
  dt.col.names <- colnames(dt)
  setcolorder(dt, c("primary.cov", dt.col.names[!(dt.col.names %in% c("primary.cov"))]))
  
  setkeyv(dt, "primary.cov") # Order by primary covariate.
  n.by.primary.cov <-
    data.table(dt[, table(primary.cov)]) # frequency by primary cov.
  
  
  if (dt[, is.integer(primary.cov)])
    n.by.primary.cov[, primary.cov := as.integer(primary.cov)]
  
  setkey(n.by.primary.cov, "primary.cov")
  setnames(n.by.primary.cov, old = "N", new = "N.in.primary.cov.group")
  
  
  # Make sure arguments are supplied in correct order. Cannot do this earlier
  # becasue the fun() allows primary.cov to be anywhere in data table, but this
  # variable is always moved to front to standardize operations
  setnames(dt, "primary.cov", arguments[[1]])
  if (!all(sapply(seq_along(arguments), function(i)
    arguments[i] == dt.col.names[i])))
    stop(
      "Please supply to arguments in the same order as they are listed in the data table input object"
    )
  
  # Grid of all possible values. This allows us to record covariate combinations
  # with zero patients in them. Will merge this with count dt later:
  unique.values <- lapply(dt, unique)
  subsetCovars <- function(x, dat) {
    dat[[x]] # Helper function for subsetting in lapply step
  }
  out.grid <- data.table(expand.grid(unique.values))
  list.allcombos <- lapply(arguments, subsetCovars, dat = out.grid)
  
  out.grid[, join.id := do.call(paste, c(list.allcombos, sep = ":"))]
  
  # Apply helper function to every covariate argument supplied by user. Output
  # is a list. Each list element is a vector where every element of vector
  # represents the covariate for one patient. i.e. vector of age of every
  # patients
  covar.list <- lapply(arguments, subsetCovars, dat = dt)
  patient.count <-
    data.table(table(do.call(paste, c(covar.list, sep = ":"))))
  
  
  # Join count dt with dt of all possible outcomes:
  setkey(out.grid, "join.id")
  setkey(patient.count, "V1")
  out.grid[, count := 0L]
  out.grid[patient.count, count := N]
  
  # Join age group count
  setkeyv(out.grid, arguments[[1]])
  out.grid[, N.in.primary.cov := 0L]
  out.grid[n.by.primary.cov, N.in.primary.cov := N.in.primary.cov.group]
  
  # Calculate proportion of patients in each covariate grouping by age.
  out.grid[, prop.by.primary.cov := count / N.in.primary.cov]
  # browser()
  out.col.names <- colnames(out.grid)
  setcolorder(out.grid, c("join.id", out.col.names[!(out.col.names %in% c("join.id"))]))
  out.grid[]
}
