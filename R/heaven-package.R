#' samplepop
#' 
#' sample data to test R versions of code from hell
#' 
#' @name samplepop
#' @docType data
#' @format A data frame with 32 observations on the following 5 variables.
#' \describe{
#' \item{pnr}{patient id}
#' \item{recno}{record number}
#' \item{inddto}{hospital admission first date}
#' \item{uddto}{hospital admission last date}
#' \item{pattype}{type of admission}
#' }
#' @keywords datasets
##' @examples
##' 
##' data(samplepop)
#'
#' @docType package
#' @name heaven
#' @useDynLib heaven
#' @importFrom Rcpp sourceCpp evalCpp
#' @importFrom data.table data.table ":=" setnames setorder setcolorder setkey rbindlist
#' @importFrom foreach "%dopar%" foreach
NULL
