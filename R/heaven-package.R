#' Public version of Framingham data
#' 
#' The Framingham dataset is a public version of several examinations of people in the Framingham study. Various variables have been adjusted to ensure anonymity of people. Thus, the data cannot be used for science, only demonstration of R-code. 
#' 
#' @name Framingham
#' @docType data
#' @format The Framingham dataset is a public version of several examinations of people in the Framingham study. Various variables have been adjusted to ensure anonymity of people. Thus, the data cannot be used for science, only demonstration of R-code. 
#' \describe{
#' \item{randid}{person code} 
#' \item{sex}{1=male 2=female} 
#' \item{totchol}{cholesterol mg/dL} 
#' \item{age}{years} 
#' \item{sysbp}{mm Hg} 
#' \item{diabp}{mm Hg} 
#' \item{cursmoke}{0=no 1=yes} 
#' \item{cigpday}{0=no 1-90 per day} 
#' \item{bmi}{body mass index} 
#' \item{diabetes}{0=no 1=yes} 
#' \item{bpmeds}{0=no 1=yes} 
#' \item{heartrte}{beats per minute} 
#' \item{glucose}{mg/dL} 
#' \item{educ}{education} 
#' \item{prevchd}{Prevalent coronary disease} 
#' \item{prevap}{prevalent angina} 
#' \item{prevmi}{prevalent MI} 
#' \item{prevstrk}{prevalent stroke} 
#' \item{prevhyp}{prevalent hypertension} 
#' \item{time}{days since baseline} 
#' \item{period}{exmination cycle} 
#' \item{hdlc}{hdl cholesterol} 
#' \item{ldlc}{ldl cholesterol} 
#' \item{death}{0=no 1=yes} 
#' \item{angina}{pectoris} 
#' \item{hospmi}{hospitalised MI} 
#' \item{MI_FCHD}{hospitalised MI or fatal coronary disease} 
#' \item{anychd}{any coronary heart disease} 
#' \item{stroke}{0=no 1=yes} 
#' \item{cvd}{cardiovascular disease} 
#' \item{hyperten}{0=no 1=yes} 
#' \item{timeap}{days til angina} 
#' \item{timemi}{days til MI} 
#' \item{timemifc}{days til MI or fatal coronary} 
#' \item{timechd}{days til coronary disease} 
#' \item{timestrk}{days til stroke} 
#' \item{timecvd}{days til cardiovascular disease} 
#' \item{timedth}{days til death} 
#' \item{timehyp}{days til hypertension} 
#'} 
#' @keywords datasets
##' @examples
##' 
##' data(Framingham)
NULL

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
