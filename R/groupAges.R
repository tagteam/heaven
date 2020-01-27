#'@title groupAges
#'
#'@description Helper function to bin a vector of continuous ages into groups
#'  and automatically generate labels.
#'@param x Vector to be grouped (usually ages in years)
#'@param lower Lower bound (inclusive) of age
#'@param upper Upper bound (exclusive) of age
#'@param by How many years to group by
#'@param sep Controls the serperator in the label between the the lower and
#'  upper bound for each age group
#'@param include.below If TRUE (default) value under the lower bound will be
#'  included and labeled accordingly. If false, values under will be returned as
#'  NA
#'@param include.above If TRUE (default) value over the upper bound will be
#'  included and labeled accordingly. If false, values over will be returned as
#'  NA
#'@param below.char The character to use for labeling the group of value under
#'  the lower bound
#'@param above.char The character to use for labeling the group of value above
#'  the lower bound
#'@return Returns a factor vector of the same length as x.
#'@author Matthew Phelps <mphelps@hjerteforeningen.dk>
#' @examples
#' library(heaven)
#' set.seed(1)
#'x <- floor(runif(20, min = 0, max = 105))
#'groupAges(
#'  x,
#'  lower = 15,
#'  upper = 50,
#'  by = 10,
#'  below.char = "<",
#'  above.char = "+"
#')

#' @export
groupAges <-
  function(x,
           lower,
           upper,
           by,
           sep = " - ",
           include.below = TRUE,
           include.above = TRUE,
           below.char = "<",
           above.char = "+") {
    if (length(lower:(upper-1)) %% by != 0) message(
        "Warning: age groups may not contain equal number of years. This probably
    means the age span between the upper and lower age limits is cannot be
    evenly divided by the group size"
      )
    labs <- c(paste(
      seq(lower, upper - by, by = by),
      seq(lower + by - 1, upper - 1, by = by),
      sep = sep
    ))
    if (all(include.below, include.above)) {
      labs <-
        c(paste0(below.char, lower), labs, paste0(upper, above.char))
      breaks <- c(-Inf, seq(lower, upper, by = by), Inf)
    } else if (include.below & !include.above) {
      labs <-
        c(paste0(below.char, lower), labs)
      breaks <- c(-Inf, seq(lower, upper, by = by))
    } else if (!include.below & include.above) {
      labs <-
        c(labs, paste0(upper, above.char))
      breaks <- c(seq(lower, upper, by = by), Inf)
    }
    cut(x,
        breaks = breaks,
        right = FALSE,
        labels = labs)
  }
