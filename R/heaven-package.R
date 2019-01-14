#' Public version of Framingham data
#' 
#' The Framingham dataset is a public version of several examinations of people in the Framingham study. Various variables have been adjusted to ensure anonymity of people. Thus, the data cannot be used for science, only demonstration of R-code. 
#' 
#' @name Framingham
#' @docType data
#' @format The Framingham dataset is a public version of several examinations of people in the Framingham study. Various variables have been adjusted to ensure anonymity of people. Thus, the data cannot be used for science, only demonstration of R-code. 
#' \describe{
#' \item{randid}{bla bla} 
#' \item{sex}{bla bla} 
#' \item{totchol}{bla bla} 
#' \item{age}{bla bla} 
#' \item{sysbp}{bla bla} 
#' \item{diabp}{bla bla} 
#' \item{cursmoke}{bla bla} 
#' \item{cigpday}{bla bla} 
#' \item{bmi}{bla bla} 
#' \item{diabetes}{bla bla} 
#' \item{bpmeds}{bla bla} 
#' \item{heartrte}{bla bla} 
#' \item{glucose}{bla bla} 
#' \item{educ}{bla bla} 
#' \item{prevchd}{bla bla} 
#' \item{prevap}{bla bla} 
#' \item{prevmi}{bla bla} 
#' \item{prevstrk}{bla bla} 
#' \item{prevhyp}{bla bla} 
#' \item{time}{bla bla} 
#' \item{period}{bla bla} 
#' \item{hdlc}{bla bla} 
#' \item{ldlc}{bla bla} 
#' \item{death}{bla bla} 
#' \item{angina}{bla bla} 
#' \item{hospmi}{bla bla} 
#' \item{MI_FCHD}{bla bla} 
#' \item{anychd}{bla bla} 
#' \item{stroke}{bla bla} 
#' \item{cvd}{bla bla} 
#' \item{hyperten}{bla bla} 
#' \item{timeap}{bla bla} 
#' \item{timemi}{bla bla} 
#' \item{timemifc}{bla bla} 
#' \item{timechd}{bla bla} 
#' \item{timestrk}{bla bla} 
#' \item{timecvd}{bla bla} 
#' \item{timedth}{bla bla} 
#' \item{timehyp}{bla bla} 
#'} 
#' @keywords datasets
##' @examples
##' 
##' data(Framingham)
#'

#' 
#' @docType package
#' @name heaven
#' @useDynLib heaven
#' @importFrom Rcpp sourceCpp evalCpp
#' @importFrom data.table fread as.data.table setkeyv is.data.table data.table ":=" setnames setorder setcolorder setkey rbindlist setkeyv set copy shift setDT
#' @importFrom foreach "%dopar%" foreach
#' @importFrom grDevices colorRampPalette topo.colors
#' @importFrom graphics points title
#' @importFrom scales date_format
#' @importFrom ggplot2 ggplot aes facet_wrap theme_bw guides geom_point geom_line guide_legend scale_fill_manual theme xlab element_rect scale_x_date unit scale_color_manual
#' @importFrom stats D predict qnorm rbinom rnorm runif setNames
#' @importFrom utils packageVersion setTxtProgressBar tail txtProgressBar
NULL
