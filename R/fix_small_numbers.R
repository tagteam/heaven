
### fix_small_numbers.R --- 
#----------------------------------------------------------------------
## Author: Sebastian Kinnberg Nielsen
## Created: April 24 2026
## Version: 
## Last-Updated:  
##           By: 
##     Update #: 0
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
# 
#----------------------------------------------------------------------
## 
### Code:  
#' @title fix_small_numbers
#' @description
#' This function is designed to anonymize observations below a certain threshold.
#' It works on a summary(univariateTable) object created using the univariateTable 
#' function from the package Publish. 
#' The fix_small_numbers function will not work as intended on any data.frame not 
#' structured the same way as a summary(univariateTable).
#' It is useful for ensuring no cells have fewer observations
#' than 3 or a custom threshold. It does not take into account cases where the
#' number of observations can be inferred. Further it is NOT meant as a
#' replacement of looking at the data manually.
#'
#' The function looks through all cells in a summary(univariateTable) object and replaces any
#' numbers below a custom threshold (e.g. 5) with "<5". It does not change any
#' values in the p-value column, nor does it change median or mean values below
#' the threshold.
#'
#' @usage
#' fix_small_numbers(my_table, threshold = 5)
#' @author Sebastian Kinnberg Nielsen
#' @param my_table The summary(univariateTable) object in which to anonymize observations.
#' @param threshold The custom threshold for which values below should be
#' changed e.g. 3 or 5. Defaults to 5.
#' @return
#' The function returns the same summary(univariateTable) object as provided in my_table,
#' but with any numeric cell value strictly below threshold replaced by
#' the string "<threshold>" (e.g. "<5"). Cells in the p-value column and
#' rows labelled "mean (sd)" or "median [iqr]" are left untouched.
#' @details
#' The function assumes that the first two columns of my_table contain
#' non-numeric information (variable names and levels) and that columns from
#' the third column onward contain the numerical summary cells to check.
#'
#' For each relevant cell, the function extracts the leading numeric value
#' (everything before the first "(" or "["), converts it to numeric, and
#' compares it to threshold. If the value is strictly smaller than
#' threshold, the full cell text is replaced by "<threshold>".
#'
#' Rows with Level equal to "mean (sd)" or "median [iqr]" are skipped, as
#' are columns named "p-value".
#' @seealso Publish::univariateTable
#' 
#' @examples
#' library(heaven)
#' library(Publish)
#' library(data.table)
#' 
#' \dontrun{library(riskCommunicator)
#'
#'
#' # Load the Framingham dataset
#' data(framingham)
#' framingham1 <- framingham[1:120,]
#'
#' # Create a univariateTable summary object stratified by sex, including 2
#' # categorical variables (CURSMOKE and PREVCHD) and 2 quantitative variables
#' # (AGE and BMI). Q() marks quantitative variables.
#' tab <- univariateTable(SEX ~ Q(AGE) + BMI + CURSMOKE + PREVCHD,
#'                        data = framingham1)
#' summary_tab <- summary(tab)
#' # Anonymize any cells with counts below 5
#' fixed_tab<-fix_small_numbers(summary_tab, threshold = 5)
#' }
#' # Note that it does NOT fix the issue that you can still 
#' # figure out the number of sex=2 based on the total column.
#' # and likewise for the 1 row for PREVCHD.
#' @export

fix_small_numbers <- function(my_table, threshold = 5)
{
  # A helper function that replaces the first numeric value in a string if < threshold
  replace_small_numbers <- function(x, threshold) {
    # 1. Remove everything from the first "(" or "[" onward
    #    So e.g. "49.1 (29)" becomes "49.1",
    #    or "2 [1-3]" becomes "2".
    num_part <- sub("[\\(\\[].*$", "", x)
    
    # 2. Convert the extracted part to numeric
    #    trimws() strips any whitespace at the start or end of the string.
    #    suppressWarnings() prevents R from printing NA-coercion warnings if num_part isn't numeric.
    num_val <- suppressWarnings(as.numeric(trimws(num_part)))
    
    # 3. Determine which entries are < threshold
    #    We define "is_small" as a logical vector that says TRUE if:
    #       - num_val is not NA, and
    #       - num_val is strictly less than 'threshold'.
    is_small <- !is.na(num_val) & num_val < threshold
    
    # 4. For those entries where is_small == TRUE, replace the full original cell text
    #    with "<threshold>", e.g. "<5" if threshold=5.
    x[is_small] <- paste0("<", threshold)
    
    # 5. Return the modified vector x
    x
  }
  
  # 6. Identify rows we do NOT want to change
  #     #    should *not* be modified (e.g., rows labeled "mean (sd)" or "median [iqr]").
  #    The %in% operator checks each element of my_table$Level to see if it matches
  #    any of the values in skip_levels.
  skip_rows <- my_table$Level %in% c("mean (sd)", "median [iqr]")
  
  # 7. Loop through columns 3 to ncol(my_table)
  #    We assume columns 1 and 2 contain non-numeric info (e.g. variable names, labels),
  #    while columns 3 onward contain the numerical summary cells to check.
  for (j in seq.int(3, ncol(my_table))) {
    
    
    # If this column's name is "p-value", skip it entirely
    if (names(my_table)[j] == "p-value") {
      next
    }
    # 8. For each column j:
    #    - Identify the subset of rows that do NOT need skipping (that is, !skip_rows)
    #    - Extract the cells in column j for those rows
    #    - Pass them to 'replace_small_numbers'
    #    - Put the returned (modified) values back into the same location of my_table.
    my_table[!skip_rows, j] <- replace_small_numbers(my_table[!skip_rows, j],
                                                     threshold = threshold)
  }
  
  # 9. Return the fully updated table
  return(my_table)
}
