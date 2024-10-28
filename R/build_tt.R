# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!
build_tt <- function(x, output = NULL) {
  output <- sanitize_output(output)

  x <- switch(output,
    html = swap_class(x, "tinytable_bootstrap"),
    latex = swap_class(x, "tinytable_tabularray"),
    markdown = swap_class(x, "tinytable_grid"),
    gfm = swap_class(x, "tinytable_grid"),
    typst = swap_class(x, "tinytable_typst"),
    dataframe = swap_class(x, "tinytable_dataframe"),
  )

  x@output <- output

  # groups must increment indices here
  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    x@nrow <- x@nrow + length(l$i)
    if (length(l$j) > 0) {
      x@nhead <- x@nhead + 1
    }
  }

  tab <- x@table_dataframe

  # strip ANSI from `tibble`/`pillar`; keep for markdown
  if (isTRUE(check_dependency("fansi"))) {
    for (col in seq_along(tab)) {
      if (isTRUE(x@output == "html")) {
        tab[[col]] <- as.character(fansi::to_html(tab[[col]], warn = FALSE))
      } else if (isTRUE(!x@output %in% c("markdown", "dataframe"))) {
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

  # data frame we trim strings, pre-padded for markdown
  if (x@output == "dataframe") {
    tmp <- x@table_dataframe
    for (i in seq_along(tmp)) {
      tmp[[i]] <- trimws(tmp[[i]])
    }
    x@table_dataframe <- tmp
  }

  # markdown styles need to be applied before creating the table, otherwise there's annoying parsing, etc.
  if (x@output %in% c("markdown", "gfm", "dataframe")) {
    for (l in x@lazy_style) {
      l[["x"]] <- x
      x <- eval(l)
    }
  }

  # draw the table
  x <- tt_eval(x)

  ihead <- 0
  for (idx in seq_along(x@lazy_group)) {
    l <- x@lazy_group[[idx]]
    l[["x"]] <- x
    if (length(l[["j"]]) > 0) {
      ihead <- ihead - 1
      l[["ihead"]] <- ihead
    }
    x <- eval(l)
  }

  if (x@output == "typst") {
    if (is.null(x@theme[[1]]) || is.function(x@theme[[1]]) || isTRUE(x@theme[[1]] %in% c("default", "striped"))) {
      # reverse the order of the lines to allow overwriting defaults
      ls <- x@lazy_style 
      x <- style_tt(x, i = nrow(x), line = "b", line_width = 0.1)
      if (x@nhead > 0) {
        x <- style_tt(x, i = -x@nhead + 1, line = "t", line_width = 0.1)
        x <- style_tt(x, i = 1, line = "t", line_width = 0.05)
      } else {
        x <- style_tt(x, i = 1, line = "t", line_width = 0.1)
      }
    }
  }

  if (!x@output %in% c("markdown", "gfm", "dataframe")) {
    for (l in x@lazy_style) {
      l[["x"]] <- x
      # output-specific styling
      if (is.null(l$output) || isTRUE(x@output == l$output)) {
        x <- eval(l)
      }
    }
  }

  if (x@output == "typst") {
    x <- style_apply_typst(x)
  } else if (x@output == "html") {
    x <- style_apply_bootstrap(x)
  } else if (x@output == "latex") {
    x <- style_apply_tabularray(x)
  }

  x <- finalize(x)

  x@table_string <- lines_drop_consecutive_empty(x@table_string)
  if (output == "gfm") {
    assert_dependency("pandoc")
    x@table_string <- paste(pandoc::pandoc_convert(text = x@table_string, to = "gfm"), collapse = "\n")
  }

  return(x)
}
