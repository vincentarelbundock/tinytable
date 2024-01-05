#' @export
spec_cell.tinytable_latex <- function(x,
                                       i,
                                       j,
                                       halign = NULL,
                                       valign = NULL,
                                       wd = NULL,
                                       fg = NULL,
                                       bg = NULL,
                                       bold = FALSE,
                                       italic = FALSE,
                                       monospace = FALSE,
                                       smallcaps = FALSE) {

  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(i, lower = 1, upper = attr(x, "nrow"), null.ok = FALSE)
  checkmate::assert_integerish(j, lower = 1, upper = attr(x, "ncol"), null.ok = FALSE)

  # Get spec from tabularray_spec function
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

  # Construct cell header
  cell_header <- sprintf(
    "cell{%s}{%s}={%s},",
    paste(i, collapse = ","),
    paste(j, collapse = ","),
    # for some reason, double-escaping is happening
    gsub("\\\\\\\\", "\\\\", gsub("\\[|\\]", "", spec))
  )

  # Find and replace the cell spec
  tab <- tabularray_setting(x, cell_header, inner = TRUE)

  # Re-build table
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)

  return(tab)
}

