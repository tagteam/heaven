#' @title sasDateRange
#'
#' @description Helper function for making sas "Where" statements for use in importSAS(). Creates date range (inclusive) in SAS formate
#' @param var.date Name of the date variable in the SAS dataset
#' @param date.start The start date of the desired date range in r date format
#' @param date.end The end date of the desired date range in r date format
#'
#' @return Function returns a string that can be passed to the "where" statement
#'   of importSAS. Alternatinvely it can be passed to makeSasWhereStatment
#'   function to be combined with further SAS where statements
#'
#' @examples
#' @export
sasDateRange <-
  function(var.date,
           date.start,
           date.end,
           inclusive = TRUE) {
    op <- c(" > '", " < '")
    if (inclusive)
      op <- c(" >= '", " <= '")
    paste0(
      paste0(var.date, op[1], rDateToSas(date.start), "'d"),
      " AND ",
      paste0(var.date, op[2], rDateToSas(date.end), "'d")
    )
  }

#' rDateToSas
#'
#' @param rdate Date object to be converted into a format SAS can read
#'
#' @return
#'
#' @examples
#' @export
rDateToSas <- function(rdate) {
  format(rdate, '%d%b%Y')
}






#' @title sasSubstr
#'
#' @description Helper function to construct a SAS 'where' statement to search through substrings in a SAS dataset via importSAS
#'
#'@usage sasSubstr(str.vec, tartget.var)
#'
#' @param str.vec Character vector where each element is the start of the
#'   substring that should be searched for. Fx. c("DI05", "DI06) will search for
#'   any recrods that start with DI05 or DI06.
#' @param target.var Character name of the variable in the SAS dataset that is
#'   to be searched
#'
#' @details The function allows a user to search for strings matching those in
#'   `str.vec` in SAS datasets. Fx it can be used to find records in the LPR that match an ICD code
#'
#' @return Returns a string that can be passed to the "where" arguments
#'   of importSAS, or combined with other conditions in sasWhereConstructor
#' @export
#' @seealso importSAS
#' @examples
#'library(heaven)
#'x <- c("DI05", "DI06")
#'sasSubstr(x, target.var)
#'
#'
sasSubstr <- function(str.vec, target.var) {
  stopifnot(is.character(str.vec))
  x <- paste0(str.vec, collapse = ", ")
  x.split <- strsplit(x, split = ", ")[[1]]
  x.nchar <- nchar(x.split)
  out <- paste0("substr (",
                target.var,
                ", 1, ",
                x.nchar,
                ") = '",
                x.split,
                "'",
                collapse = " OR ")
  paste0("(", out, ")")
}


#' sasWhereConstructor
#'
#' @description Simple helper function that combines multiple SAS where statements with "AND"
#' @param vec.where A vector of valid SAS where statements
#'
#' @return Returns a string that can be passed to the "where" argument of importSAS
#' @examples
#' @export
sasWhereConstructor <- function(..., collapse = " AND ") {
  dots <- list(...)
  paste0(dots, collapse = collapse)
}
