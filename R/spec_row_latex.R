#' @export
spec_row.tinytable_latex <- function(x,
                                     i,
                                     halign = NULL,
                                     valign = NULL,
                                     wd = NULL,
                                     fg = NULL,
                                     bg = NULL,
                                     bold = FALSE,
                                     italic = FALSE,
                                     monospace = FALSE,
                                     smallcaps = FALSE) {

  rows <- attr(x, "tabularray_rows")
  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(i, lower = 1, upper = length(rows), null.ok = FALSE)

  # Get spec from tabularray_spec function (same as used in style_columns_latex)
  spec <- tabularray_spec(
    bold = bold,
    italic = italic,
    monospace = monospace,
    smallcaps = smallcaps,
    fg = fg,
    bg = bg,
    wd = wd,
    halign = halign
  )

  # Update rows
  rows[i] <- sub("\\[.*\\]", spec, rows[i])
  rows_string <- paste(rows, collapse = "")

  # Write rowspec= header
  tab <- strsplit(x, "\n")[[1]]
  idx <- grep("^rowspec=\\{", tab)
  tab[idx] <- sprintf("rowspec={%s},", rows_string)

  # Re-build table

  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)
  attr(tab, "tabularray_rows") <- rows


  return(tab)
}



