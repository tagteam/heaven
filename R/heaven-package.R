#' sample data for examples
#' 
#' a sample data frame used to illustrate some functionality
#' 
#' @name samplePPIData
#' @docType data
#' @format This data frame contains 12 observations from one subject:
#' \describe{
#' \item{pnr}{pnr number}
#' \item{eksd}{date of purchase}
#' \item{packsize}{package size}
#' \item{strnum}{strnum}
#' \item{apk}{apk}
#' \item{atc}{atc code}
#'} 
#' @keywords datasets
##' @examples
##' 
##' data(samplePPIData)
#'

#' 
#' @docType package
#' @name heaven
#' @useDynLib heaven
#' @importFrom Rcpp sourceCpp evalCpp
#' @importFrom data.table data.table ":=" setnames setorder setcolorder setkey rbindlist
#' @importFrom foreach "%dopar%" foreach
NULL
