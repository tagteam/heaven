#' Checks univariate tables for microdata
#'
#' @param dat Table to be checked. Must be the summarized output of Publish::utable()
#' @param min.val The minimum value allowed, anything below this value will be flagged as microdata
#'
#' @return A message showing the location of any microdata found, or a statement that no microdata was found
#' @export
#'
#' @examples
#' checkMicroData(summary(Publish::utable(cyl ~ vs, data=mtcars)))
checkMicroData <- function(dat, min.val = 5) {
  if (class(dat)[1] != "summary.univariateTable")
    stop("Input dataset must be of class 'summary.univariateTable', which is 
         the output of summary(utable()) (see 'Publish' package)")
  
  warn.msg <- paste0("!! numeric value <", min.val)
  d <- data.table::copy(dat)
  data.table::setDT(d)
  col.names <- tolower(colnames(d))
  cols.to.test <- !grepl("^variable|^level|p-value", col.names)
  cols.test.names <- col.names[cols.to.test]
  cols.ref <- grepl("^variable|^level", col.names)
  
  # Loop over columns
  tmp <- d[, ..cols.to.test][, lapply(.SD, function(k) {
    # Loop over cells
    for (i in seq_len(length(k))) {
      # Remove thousands sep (,) and removes anything after first number in cell
      k[i] <- gsub(",|\\s+.*", "", k[i])
    }
    k <- as.numeric(k)
    inx <- k < min.val
    out <- rep("ok", length(inx))
    out[inx] <- warn.msg
    out
    
  })]
  
  out.flag <- any(vapply(tmp, function(i) {
    any(i != "ok")
  }, logical(1)))
  if (out.flag) {
    cbind(d[, ..cols.ref], tmp)
  } else {
    
    
    message(strwrap(
      cat(paste0(
        "No values <",
        min.val,
        " found in the following columns: "), 
        paste0(cols.test.names, collapse = "\n"),
        sep = "\n"),
      prefix = "   ",
      initial = ""
    ))
  }
  
}