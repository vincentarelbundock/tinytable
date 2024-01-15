#' Save a Tiny Table to File
#'
#' This function saves an object of class tinytable_bootstrap, tinytable_tabularray, 
#' or tinytable_markdown to a specified file, with an option to overwrite existing files.
#'
#' @param x The tinytable object to be saved.
#' @param filename A string representing the path to the file where the object should be saved.
#' @param overwrite A logical value indicating whether to overwrite an existing file. 
#' @return invisible(TRUE)
#' @export
#' @examples
#' \dontrun{
#'
#' library(tinytable)
#' tab  <- tt(mtcars[1:4, 1:4])
#' save_tt(tt, "path/to/file.txt")
#'
#' }
#'
save_tt <- function(x, filename, overwrite = FALSE) {

  m <- meta(x)


  assert_string(filename)
  assert_flag(overwrite)
  if (file.exists(filename) && !overwrite) {
    stop("File already exists and overwrite is set to FALSE.", call. = FALSE)
  }
  if (is.null(m)) {
    stop("`x` must be an object produced by the `tinytable::tt()` function.", call. = FALSE)
  }

  write(x, file = filename)

  return(invisible(TRUE))
}

