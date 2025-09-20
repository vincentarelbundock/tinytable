#' Theme for a void table
#'
#' This function calls `strip_tt()` to remove all the styles, groups, and formatting applied to a `tinytable` object. It returns a nearly blank table, with only the cell information. Warning: since this function strips the `tinytable` object, the order in which it is called in a pipeline matters.
#' @inheritParams theme_default
#' @export
theme_empty <- function(x, ...) {
  # strip everything done up to this point
  strip <- names(formals(strip_tt))
  strip <- stats::setNames(c(list(x), rep(list(TRUE), length(strip) - 1)), strip)
  x <- do.call(strip_tt, strip)

  x <- theme_markdown(x, vline = FALSE, hline = FALSE)

  fn <- function(table) {
    s <- table@table_string
    s <- gsub("\\\\toprule|\\\\bottomrule|\\\\midrule", "", s)
    l <- strsplit(s, "\n")[[1]]
    l <- l[which(trimws(l) != "")]
    table@table_string <- paste(l, collapse = "\n")
    return(table)
  }
  x <- build_finalize(x, fn, output = "latex")

  fn <- function(table) {
    tab <- table@table_string
    tab <- strsplit(tab, "\n")[[1]]
    # Only remove horizontal lines if grid_hline is FALSE
    if (isFALSE(table@grid_hline)) {
      tab <- tab[!grepl("^[\\+|-]+$", tab)]
      tab <- tab[!grepl("^[\\+|=]+$", tab)]
    }
    # Only remove vertical lines if grid_vline is FALSE
    if (isFALSE(table@grid_vline)) {
      tab <- gsub("|", " ", tab, fixed = TRUE)
    }
    table@table_string <- paste(tab, collapse = "\n")
    return(table)
  }
  x <- build_finalize(x, fn, output = "markdown")

  return(x)
}
