plot_tt <- function(x,
                    i = NULL,
                    j = NULL,
                    data = NULL,
                    path = NULL,
                    height = 2,
                    ...) {

  assert_integerish(i, null.ok = TRUE)
  assert_integerish(j, null.ok = TRUE)
  assert_integerish(height, len = 1)
  assert_class(x, "tinytable")
  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  len <- length(ival) * length(jval)
  assert_character(path, len = len)
  if (length(path) != len) {
    msg <- sprintf("`path` must match the dimensions of `i` and `j`: length %s.", len) 
    stop(msg, call. = FALSE)
  }

  # needed when rendering in tempdir()
  out <- meta(out, "path_plot", path)

  cal <- call("plot_tt_lazy", 
    i = ival,
    j = jval,
    path = path,
    data = data,
    height = height)

  out <- meta(out, "lazy_plot", c(meta(out)$lazy_plot, list(cal)))
  out <- meta(out, "path_image", path)

  return(out)
}


plot_tt_lazy <- function(x,
                         i = NULL,
                         j = NULL,
                         path = NULL,
                         data = NULL,
                         height = 2,
                         assets = "tinytable_assets",
                         ...) {

  out <- x

  if (!is.null(data)) {
    assert_dependency('ggplot2')
    path <- NULL
    if (!is.null(meta(x)$output_dir)) {
      assets_full <- file.path(meta(x)$output_dir, assets)
    }

    if (!dir.exists(assets_full)) {
      dir.create(assets_full)
    }
    for (idx in seq_along(data)) {
      fn <- paste0(get_id(), ".png")
      fn_full <- file.path(assets_full, fn)
      fn <- file.path(assets, fn)
      path[idx] <- fn
      d <- data.frame(x = data[[idx]])
      p <- ggplot2::ggplot(d, ggplot2::aes(x = x)) + 
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::theme_void()
      suppressMessages(ggplot2::ggsave(
        filename = fn_full,
        height = 100, width = 200,
        units = "px"
      ))
    }
  }

  if (meta(x)$output == "latex") {
    cell <- "\\includegraphics[height=%sem]{%s}"
    cell <- sprintf(cell, height, path)

  } else if (meta(x)$output == "html") {
    cell <- ifelse(
      grepl("^http", trimws(path)),
      '<img src="%s" style="height: %sem;">',
      '<img src="./%s" style="height: %sem;">')
    cell <- sprintf(cell, path, height)

  } else if (meta(x)$output == "markdown") {
    cell <- '![](%s)'
    cell <- sprintf(cell, path)

  } else {
    stop("here be dragons")
  }

  out[i, j] <- cell


  return(out)
}



