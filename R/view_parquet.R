#' View a Parquet File or Directory
#'
#' Quickly inspect the contents of a Parquet file or a directory of Parquet
#' files without reading the whole thing into memory. `action = "head"` shows
#' the first `num_rows` rows (all columns), while `action = "str"` prints a
#' transposed structure summary via [dplyr::glimpse()].
#'
#' The function uses [arrow::open_dataset()], which is lazy: only the row groups
#' needed to satisfy the request are read from disk. This makes it fast even on
#' very large files or multi-file directories.
#'
#' @param path Path to a Parquet file or a directory of Parquet files. Windows
#'   backslash paths are accepted and normalised automatically.
#' @param action Either `"head"` to show the first few rows, or `"str"` for a
#'   structure summary. Defaults to `"head"`.
#' @param num_rows Number of rows to display when `action = "head"`. Default 5.
#' @param quiet Logical; if `FALSE` (default) the resolved path is printed.
#'
#' @return Invisibly returns the collected `tibble` (the head, or the sample
#'   used for the structure summary), so it can be captured if needed. Output is
#'   printed as a side effect.
#'
#' @examples
#' \dontrun{
#' view_parquet("path/to/file.parquet", action = "str")
#' # Windows raw-string path:
#' view_parquet(r"(Z:\path\to\directory)", action = "head")
#' }
#'
#' @importFrom arrow open_dataset
#' @importFrom dplyr collect glimpse
#' @importFrom utils head
#' @export
view_parquet <- function(path,
                         action = c("head", "str"),
                         num_rows = 5,
                         quiet = FALSE) {
  action <- match.arg(action)

  # Normalise Windows backslashes to forward slashes
  normalized_path <- gsub("\\\\", "/", path)

  if (!file.exists(normalized_path) && !dir.exists(normalized_path)) {
    stop("The specified path does not exist: ", normalized_path)
  }

  if (!quiet) {
    message("Reading: ", normalized_path)
  }

  # open_dataset() is lazy for both a single file and a directory of parquet
  # files, so nothing is read until collect().
  ds <- arrow::open_dataset(normalized_path, format = "parquet")

  if (action == "head") {
    df <- dplyr::collect(head(ds, num_rows))
    # width = Inf forces all columns to print instead of being truncated to
    # the console width (the cause of only seeing one "line").
    print(df, width = Inf, n = num_rows)
  } else { # "str"
    # Grab a small sample so glimpse has data types/example values without
    # scanning the whole dataset.
    df <- dplyr::collect(head(ds, num_rows))
    dplyr::glimpse(df)
  }

  invisible(df)
}
