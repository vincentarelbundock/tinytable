# internal function
# style_tt() stores style calls and we only want to evaluate them at the end because 
# some rows may be added, which changes how the style is applied
#
# THE ORDER MATTERS A LOT!
build_tt <- function(x, output = NULL) {
  m <- meta(x)

  output <- sanitize_output(output)
  out <- x
  out <- meta(out, "output", output)

  # format data before drawing the table
  for (l in m$lazy_format) {
    tmp <- out
    class(tmp) <- "data.frame"
    l[["x"]] <- tmp
    out <- eval(l)
  }

  # plots and images
  for (l in m$lazy_plot) {
    tmp <- out
    class(tmp) <- "data.frame"
    l[["x"]] <- tmp
    out <- eval(l)
  }

  # markdown styles need to be applied before creating the table, otherwise there's annoying parsing, etc.
  if (output == "markdown") {
    for (l in m$lazy_style) {
      l[["x"]] <- out
      out <- eval(l)
    }
  }

  # shouldn't have to add this everywhere, but I'm too lazy to check
  out <- meta(out, "output", output)

  # draw the table
  lazy_tt <- meta(x, "lazy_tt")
  lazy_tt[["x"]] <- out
  if (output == "html") {
    lazy_tt[[1]] <- quote(tt_bootstrap)
  } else if (output == "latex") {
    lazy_tt[[1]] <- quote(tt_tabularray)
  } else if (output == "markdown") {
    lazy_tt[[1]] <- quote(tt_grid)
  }
  out <- eval(lazy_tt)
  out <- meta(out, "output", output)

  # group the table (before style)
  for (l in m$lazy_group) {
    l[["x"]] <- out
    if (output == "html") {
      l[[1]] <- quote(group_bootstrap)
    } else if (output == "latex") {
      l[[1]] <- quote(group_tabularray)
    } else if (output == "markdown") {
      l[[1]] <- quote(group_grid)
    }
    out <- eval(l)
  }
  out <- meta(out, "output", output)

  # style the table
  if (output != "markdown") {
    for (l in m$lazy_style) {
      l[["x"]] <- out
      out <- eval(l)
    }
  }

  # finalize 
  out <- finalize_bootstrap(out)

  # formal grid specification in pandoc includes lines everywhere
  hlines <- getOption("tinytable_grid_hlines", default = TRUE)
  if (isTRUE(hlines) && output == "markdown") {
    out <- grid_hlines(out)
  }

  m <- meta(x)
  m$lazy_style <- list()
  attr(out, "tinytable_meta") <- m
  out <- meta(out, "output", output)

  return(out)
}


finalize_bootstrap <- function(x) {
    if (meta(x)$output != "html") return(x)
    out <- gsub(
      "$tinytable_BOOTSTRAP_CLASS",
      "table",
      x,
      fixed = TRUE)
    return(out)
}
