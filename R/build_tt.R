# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because 
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!
build_tt <- function(x, output = NULL) {

  x@output <- sanitize_output(output)

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
  args <- list(x = x, caption = x@caption, theme = x@theme, width = x@width, notes = x@notes, placement = x@placement)
  fun <- switch(x@output,
    html = tt_bootstrap,
    latex = tt_tabularray,
    markdown = tt_grid,
    typst = tt_typst
  )
  x <- do.call(fun, args)

  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    l[["x"]] <- x
    l[["ihead"]] <- -1 * idx
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




