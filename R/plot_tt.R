plot_tt <- function(x,
                    i = NULL,
                    j = NULL,
                    height = 2,
                    data = NULL,
                    fun = NULL,
                    path_img = NULL,
                    path_assets = "tinytable_assets",
                    ...) {

  assert_integerish(i, null.ok = TRUE)
  assert_integerish(j, null.ok = TRUE)
  assert_numeric(height, len = 1, lower = 0)
  assert_class(x, "tinytable")
  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  len <- length(ival) * length(jval)
  assert_character(path_img, len = len)
  if (length(path_img) != len) {
    msg <- sprintf("`path_img` must match the dimensions of `i` and `j`: length %s.", len) 
    stop(msg, call. = FALSE)
  }

  # needed when rendering in tempdir()
  out <- meta(out, "path_plot", path_img)

  cal <- call("plot_tt_lazy", 
    i = ival,
    j = jval,
    data = data,
    fun = fun,
    height = height,
    path_img = path_img,
    path_assets = path_assets)

  out <- meta(out, "lazy_plot", c(meta(out)$lazy_plot, list(cal)))
  out <- meta(out, "path_image", path_img)

  return(out)
}


plot_tt_lazy <- function(x,
                         i = NULL,
                         j = NULL,
                         fun = fun,
                         path_img = NULL,
                         path_assets = "tinytable_assets",
                         data = NULL,
                         height = 2,
                         ...) {

  out <- x

  if (!is.null(data)) {
    assert_dependency('ggplot2')
    path_img <- NULL
    if (!is.null(meta(x)$output_dir)) {
      path_full <- file.path(meta(x)$output_dir, path_assets)
    }

    if (!dir.exists(path_full)) {
      dir.create(path_full)
    }
    for (idx in seq_along(data)) {
      fn <- paste0(get_id(), ".png")
      fn_full <- file.path(path_full, fn)
      fn <- file.path(path_assets, fn)
      path_img[idx] <- fn
      p <- fun[[idx]](data[[idx]])

      # ggplot2
      if (inherits(p, "ggplot")) {
        assert_dependency("ggplot2")
        suppressMessages(ggplot2::ggsave(
          filename = fn_full,
          height = 4, width = 5,
          units = "in"
        ))

      # base R
      } else if (is.function(p)) {
        png(fn_full)
        p()
        dev.off()

      # sanity check
      } else {
        msg <- "The functions in the `fun` list must return a function or a `ggplot2` object. See the tutorial online for examples: https://vincentarelbundock.github.io/tinytable"
        stop(msg, call. = FALSE)
      }

    }
  }

  if (meta(x)$output == "latex") {
    cell <- "\\includegraphics[height=%sem]{%s}"
    cell <- sprintf(cell, height, path_img)

  } else if (meta(x)$output == "html") {
    cell <- ifelse(
      grepl("^http", trimws(path_img)),
      '<img src="%s" style="height: %sem;">',
      '<img src="./%s" style="height: %sem;">')
    cell <- sprintf(cell, path_img, height)

  } else if (meta(x)$output == "markdown") {
    cell <- '![](%s)'
    cell <- sprintf(cell, path_img)

  } else {
    stop("here be dragons")
  }

  out[i, j] <- cell


  return(out)
}



