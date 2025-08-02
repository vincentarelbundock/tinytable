#' Theme for a void table
#' @inheritParams theme_default
#' @export
theme_void <- function(x, ...) {
  x <- theme_html(x, class = "table table-borderless")

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
