#' Theme for a void table
#' @inheritParams theme_default
#' @export
theme_void <- function(x, ...) {
  # strip everything done up to this point
  strip <- names(formals(strip_tt))
  strip <- setNames(c(list(x), rep(list(TRUE), length(strip) - 1)), strip)
  x <- do.call(strip_tt, strip)

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
    tab <- tab[!grepl("^[\\+|-]+$", tab)]
    tab <- tab[!grepl("^[\\+|=]+$", tab)]
    tab <- gsub("|", " ", tab, fixed = TRUE)
    table@table_string <- paste(tab, collapse = "\n")
    return(table)
  }
  x <- build_finalize(x, fn, output = "markdown")

  return(x)
}
