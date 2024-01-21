plot_tt <- function(x,
                    i = NULL,
                    j = NULL,
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
    height = height)

  out <- meta(out, "lazy_plot", c(meta(out)$lazy_plot, list(cal)))
  out <- meta(out, "path_image", path)

  return(out)
}


plot_tt_lazy <- function(x,
                         i = NULL,
                         j = NULL,
                         path = NULL,
                         height = 2,
                         ...) {

  out <- x

  build_dir <- meta(out, "path_dir_build")

  if (!is.null(build_dir)) {
    tmp <- file.copy(
      from = path, 
      to = build_dir,
      overwrite = TRUE)
    path <- sapply(path, basename)
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



