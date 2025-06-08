theme_multipage <- function(
  x,
  rowhead = get_option("tinytable_theme_multipage_rowhead", 0L),
  rowfoot = get_option("tinytable_theme_multipage_rowfoot", 0L),
  ...
) {
  if (rowhead > 0) {
    x <- style_tt(
      x,
      tabularray_inner = sprintf("rowhead=%s", rowhead)
    )
  }

  if (rowfoot > 0) {
    x <- style_tt(
      x,
      tabularray_inner = sprintf("rowfoot=%s", rowfoot)
    )
  }

  # do not change the defaul theme
  if (identical(x@theme[[1]], "multipage")) x@theme <- list("default")
  assert_integerish(rowhead, lower = 0, len = 1)
  assert_integerish(rowfoot, lower = 0, len = 1)
  # cap <- sprintf("caption={%s}", x@caption)
  # x@caption <- ""
  fn <- function(table) {
    if (!isTRUE(table@output == "latex")) {
      return(table)
    }

    tab <- table@table_string
    tab <- sub("\\\\begin\\{talltblr", "\\\\begin\\{longtblr", tab)
    tab <- sub("\\\\end\\{talltblr", "\\\\end\\{longtblr", tab)

    tab <- strsplit(tab, "\n")[[1]]
    idx <- grepl(
      "^\\\\caption\\{|^\\\\begin\\{table|^\\\\end\\{table|^\\\\centering",
      trimws(tab)
    )
    tab <- tab[!idx]
    tab <- paste(tab, collapse = "\n")

    table@table_string <- tab

    # table <- style_tt(table, tabularray_outer = cap)

    return(table)
  }
  x <- style_tt(x, finalize = fn)
  return(x)
}
