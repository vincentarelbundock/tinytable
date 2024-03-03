# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because 
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!
build_tt <- function(x, output = NULL) {

  x@output <- sanitize_output(output)

  x <- switch(x@output,
    html = swap_class(x, "tinytable_bootstrap"),
    latex = swap_class(x, "tinytable_tabularray"),
    markdown = swap_class(x, "tinytable_grid"),
    typst = swap_class(x, "tinytable_typst")
  )

  # groups must increment indices here
  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    x@nrow <- x@nrow + length(l$i)
    if (length(l$j) > 0) {
      x@nhead <- x@nhead + 1
    }
  }

  tab <- x@table_dataframe

  # strip ANSI from `tibble`/`pillar`
  if (isTRUE(check_dependency("fansi"))) {
    for (col in seq_along(tab)) {
      if (isTRUE(x@output == "html")) {
        tab[[col]] <- as.character(fansi::to_html(tab[[col]], warn = FALSE))
      } else {
        tab[[col]] <- as.character(fansi::strip_ctl(tab[[col]]))
      }
    }
  }
  x@table_dataframe <- tab

  # format data before drawing the table
  for (l in x@lazy_format) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # add footnote markers just after formatting, otherwise appending converts to string
  x <- footnote_markers(x)

  # plots and images
  for (l in x@lazy_plot) {
    l[["x"]] <- x
    x <- eval(l)
  }

  # markdown styles need to be applied before creating the table, otherwise there's annoying parsing, etc.
  if (x@output == "markdown") {
    for (l in x@lazy_style) {
      l[["x"]] <- x
      x <- eval(l)
    }
  }

  # draw the table
  fun <- switch(x@output,
    html = tt_bootstrap,
    latex = tt_tabularray,
    markdown = tt_grid,
    typst = tt_typst
  )
  x <- fun(x)

  ihead <- 0
  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    l[["x"]] <- x
    if (length(l[["j"]]) > 0) {
      ihead <- ihead - 1
      l[["ihead"]] <- ihead
    }
    if (x@output == "html") {
      l[[1]] <- quote(group_bootstrap)
    } else if (x@output == "latex") {
      l[[1]] <- quote(group_tabularray)
    } else if (x@output == "markdown") {
      l[[1]] <- quote(group_grid)
    } else if (x@output == "typst") {
      l[[1]] <- quote(group_typst)
    }
    x <- eval(l)
  }

  # style the table
  if (x@output == "typst") {
    # rules of precedence appear to differ
    x@lazy_style <- rev(x@lazy_style)
  }

  if (x@output != "markdown") {
    for (l in x@lazy_style) {
      l[["x"]] <- x
      x <- eval(l)
    }
  }

  # finalize 
  x <- finalize_bootstrap(x)
  x <- finalize_typst(x)
  x <- finalize_grid(x)

  return(x)
}




