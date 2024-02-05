#' Insert images and inline plots into tinytable objects
#'
#' The `plot_tt()` function allows for the insertion of images and inline plots into 
#' tinytable objects. This function can handle both local and web-based images.
#'
#' @param x A tinytable object.
#' @param i Integer vector, the row indices where images are to be inserted. If `NULL`, 
#'    images will be inserted in all rows.
#' @param j Integer vector, the column indices where images are to be inserted. If `NULL`,
#'    images will be inserted in all columns.
#' @param height Numeric, the height of the images in the table in em units.
#' @param asp Numeric, aspect ratio of the plots (height / width). 
#' @param color string Name of color to use for inline plots (passed to the `col` argument base `graphics` plots in `R`).
#' @param xlim Numeric vector of length 2.
#' @param fun  A function to generate plots from the data in `data`. Valid functions include:
#' - Functions that return `ggplot2` objects.
#' - Functions that return another function which generates a base `R` plot, ex: `function(x) {function() hist(x)}`
#' - See the tutorial on the `tinytable` website for more information.
#' @param data a list of data frames or vectors to be used by the plotting functions in `fun`.
#' @param images Character vector, the paths to the images to be inserted. Paths are relative to the main table file or Quarto (Rmarkdown) document.
#' @param assets Path to the directory where generated assets are stored. This path is relative to the location where a table is saved.
#' @param ... Extra arguments are passed to the function in `fun`. Important: Custom plotting functions must always have `...` as an argument.
#'
#' @return A modified tinytable object with images or plots inserted.
#'
#' @details The `plot_tt()` can insert images and inline plots into tables.
#' @export
plot_tt <- function(x,
                    i = NULL,
                    j = NULL,
                    fun = NULL,
                    data = NULL,
                    color = "black",
                    xlim = NULL,
                    height = 1,
                    asp = 1/3,
                    images = NULL,
                    assets = "tinytable_assets",
                    ...) {

  out <- x


  j <- sanitize_j(j, x)
  assert_integerish(i, null.ok = TRUE)
  assert_numeric(height, len = 1, lower = 0)
  assert_numeric(asp, len = 1, lower = 0, upper = 1)
  assert_class(x, "tinytable")

  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j
  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i

  len <- length(ival) * length(jval)

  assert_list(data, len = len, null.ok = TRUE)
  assert_character(images, len = len, null.ok = TRUE)

  if (!is.null(images) && length(images) != len) {
    msg <- sprintf("`images` must match the dimensions of `i` and `j`: length %s.", len) 
    stop(msg, call. = FALSE)
  }

  if (!is.null(fun) && !is.null(images)) {
    stop("`fun` and `images` cannot be used together.", call. = FALSE)
  }

  if (!is.null(fun) && is.null(data)) {
    stop("Please specify `data`.", call. = FALSE)
  }

  if (is.character(fun)) {
    assert_choice(fun, c("histogram", "density", "bar", "line"))
  } else {
    assert_function(fun, null.ok = TRUE)
  }

  # built-in plots
  if (identical(fun, "histogram")) {
    fun <- rep(list(tiny_histogram), length(data))

  } else if (identical(fun, "density")) {
    fun <- rep(list(tiny_density), length(data))

  } else if (identical(fun, "line")) {
    fun <- rep(list(tiny_line), length(data))

  } else if (identical(fun, "bar")) {
    for (idx in seq_along(data)) {
      assert_numeric(data[[idx]], len = 1, name = "data[[1]]")
    }
    if (is.null(xlim)) {
      xlim <- c(0, max(unlist(data)))
    }
    fun <- rep(list(tiny_bar), length(data))
  } else {
    fun <- rep(list(fun), length(data))
  }

  # needed when rendering in tempdir()
  out <- meta(out, "path_plot", images)

  cal <- list(
    "plot_tt_lazy", 
    i = ival,
    j = jval,
    asp = asp,
    data = data,
    fun = fun,
    color = color,
    xlim = xlim,
    height = height,
    images = images,
    assets = assets)
  cal <- c(cal, list(...))
  cal <- do.call(call, cal)

  out <- meta(out, "lazy_plot", c(meta(out)$lazy_plot, list(cal)))
  out <- meta(out, "path_image", images)

  return(out)
}


plot_tt_lazy <- function(x,
                         i = NULL,
                         j = NULL,
                         height = 1,
                         asp = 1/3,
                         fun = NULL,
                         color = NULL,
                         data = NULL,
                         xlim = NULL,
                         images = NULL,
                         assets = "tinytable_assets",
                         ...) {

  out <- x

  if (!is.null(data)) {
    assert_dependency('ggplot2')
    images <- NULL
    if (!is.null(meta(x)$output_dir)) {
      path_full <- file.path(meta(x)$output_dir, assets)
    }

    if (!dir.exists(path_full)) {
      dir.create(path_full)
    }
    for (idx in seq_along(data)) {
      fn <- paste0(get_id(), ".png")
      fn_full <- file.path(path_full, fn)
      fn <- file.path(assets, fn)
      images[idx] <- fn

      plot_fun <- fun[[idx]]
      if (!"..." %in% names(formals(plot_fun))) {
        stop("Inline plotting function must have `...` as argument. See tutorial on the `tinytable` website for examples.", call. = FALSE)
      }
      p <- plot_fun(data[[idx]], xlim = xlim, color = color, ...)

      # ggplot2
      if (inherits(p, "ggplot")) {
        assert_dependency("ggplot2")
        suppressMessages(ggplot2::ggsave(
          p,
          filename = fn_full,
          width = 1, height = asp,
          units = "in"
        ))

      # base R
      } else if (is.function(p)) {
        grDevices::png(fn_full, width = 1000, height = 1000 * asp)
        op <- graphics::par()
        graphics::par(mar = c(0, 0, 0, 0))
        p()
        graphics::par(mar = op$mar)
        grDevices::dev.off()

      # sanity check
      } else {
        msg <- "The functions in the `fun` list must return a function or a `ggplot2` object. See the tutorial online for examples: https://vincentarelbundock.github.io/tinytable"
        stop(msg, call. = FALSE)
      }

    }
  }

  if (isTRUE(meta(x)$output == "latex")) {
    cell <- "\\includegraphics[height=%sem]{%s}"
    cell <- sprintf(cell, height, images)

  } else if (isTRUE(meta(x)$output == "html")) {
    cell <- ifelse(
      grepl("^http", trimws(images)),
      '<img src="%s" style="height: %sem;">',
      '<img src="./%s" style="height: %sem;">')
    cell <- sprintf(cell, images, height)

  } else if (isTRUE(meta(x)$output == "markdown")) {
    cell <- '![](%s)'
    cell <- sprintf(cell, images)

  } else {
    stop("here be dragons")
  }

  out[i, j] <- cell

  return(out)
}


tiny_histogram <- function(d, color = "black", ...) {
  function() graphics::hist(d, col = color, axes = FALSE, ann = FALSE)
}


tiny_density <- function(d, color = "black", ...) {
  function() {
    d <- stats::density(stats::na.omit(d))
    graphics::plot(d, axes = FALSE, ann = FALSE, col = color)
    graphics::polygon(d, col = color)
  }
}


tiny_bar <- function(d, color = "black", xlim = 0:1, ...) {
  function() {
    graphics::barplot(d, horiz = TRUE, col = color, xlim = xlim)
  }
}


tiny_line <- function(d, xlim = 0:1, color = "black", ...) {
  function() {
    if (!inherits(d, "data.frame") || !"x" %in% names(d) || !"y" %in% names(d)) {
      stop("The data to plot a `line` must be a data frame with columns `x` and `y`.", call. = FALSE)
    }
    plot(d$x, d$y, type = "l", col = color, axes = FALSE, ann = FALSE, lwd = 50)
  }
}
