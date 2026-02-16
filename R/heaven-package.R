#' heaven: Tools for Statitics Denmark
#' @keywords internal
"_PACKAGE"
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
