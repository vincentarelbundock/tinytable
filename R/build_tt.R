# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because 
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!
build_tt <- function(x, output = NULL) {
  output <- sanitize_output(output)
  out <- x@table

  # strip ANSI from `tibble`/`pillar`
  if (isTRUE(check_dependency("fansi"))) {
    for (col in seq_along(out)) {
      if (isTRUE(output == "html")) {
        out[[col]] <- as.character(fansi::to_html(out[[col]], warn = FALSE))
      } else {
        out[[col]] <- as.character(fansi::strip_ctl(out[[col]]))
      }
    }
  }

  # format data before drawing the table
  for (l in x@lazy_format) {
    tmp <- out
    l[["x"]] <- tmp
    out <- eval(l)
  }

  # add footnote markers just after formatting, otherwise appending converts to string
  out <- footnote_markers(out)

  # plots and images
  for (l in x@lazy_plot) {
    tmp <- out
    class(tmp) <- "data.frame"
    l[["x"]] <- tmp
    out <- eval(l)
  }

  # markdown styles need to be applied before creating the table, otherwise there's annoying parsing, etc.
  if (output == "markdown") {
    for (l in x@lazy_style) {
      l[["x"]] <- out
      out <- eval(l)
    }
  }

  # draw the table
  args <- list(x = out, caption = x@caption, theme = x@theme, width = x@width, notes = x@notes, placement = x@placement)
  fun <- switch(output,
    html = tt_bootstrap,
    latex = tt_tabularray,
    markdown = tt_grid,
    typst = tt_typst
  )
  out <- do.call(fun, args)

  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    l[["x"]] <- out
    l[["ihead"]] <- -1 * idx
    if (output == "html") {
      l[[1]] <- quote(group_bootstrap)
    } else if (output == "latex") {
      l[[1]] <- quote(group_tabularray)
    } else if (output == "markdown") {
      l[[1]] <- quote(group_grid)
    } else if (output == "typst") {
      l[[1]] <- quote(group_typst)
    }
    out <- eval(l)
  }

  # style the table
  if (output == "typst") {
    # rules of precedence appear to differ
    x@lazy_style <- rev(x@lazy_style)
  }
  if (output != "markdown") {
    for (l in x@lazy_style) {
      l[["x"]] <- out
      out <- eval(l)
    }
  }

  # finalize 
  out <- finalize_bootstrap(out)
  out <- finalize_typst(out)
  out <- finalize_grid(out)

  attributes(out) <- NULL
  x@table <- out

  return(x)
}




