#' @export
spec_column.tinytable_latex <- function(x,
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

  cols <- attr(x, "tabularray_cols")
  checkmate::assert_class(x, classes = "tinytable_latex")
  checkmate::assert_integerish(j, lower = 1, upper = length(cols), null.ok = FALSE)

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

  # Update columns
  cols[j] <- sub("\\[.*\\]", spec, cols[j])
  cols_string <- paste(cols, collapse = "")

  # Write colspec= header
  tab <- strsplit(x, "\n")[[1]]
  idx <- grep("^colspec=\\{", tab)
  tab[idx] <- sprintf("colspec={%s},", cols_string)

  # Re-build table
  tab <- paste(tab, collapse = "\n")
  attributes(tab) <- attributes(x)
  attr(tab, "tabularray_cols") <- cols

  return(tab)
}


