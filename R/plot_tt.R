#' Insert images and inline plots into tinytable objects
#'
#' The `plot_tt()` function allows for the insertion of images and inline plots into 
#' tinytable objects. This function can handle both local and web-based images. #'
#'
#' @param x A tinytable object.
#' @param i Integer vector, the row indices where images are to be inserted. If `NULL`, 
#'    images will be inserted in all rows.
#' @param j Integer vector, the column indices where images are to be inserted. If `NULL`,
#'    images will be inserted in all columns.
#' @param height Numeric, the height of the images in the table. Default is 2.
#' @param plot_data Optional, a list of data frames to be used with a custom plotting function.
#' @param plot_fun Optional, a list of functions to generate plots from the data in `plot_data`. Valid functions include:
#' - Functions that return `ggplot2` objects.
#' - Functions that return another function which generates a base `R` plot, ex: `function(x) {function() hist(x)}`
#' - See the tutorial on the `tinytable` website for more information.
#' @param path_img Character vector, the paths to the images to be inserted. Paths are relative to the main table file or Quarto (Rmarkdown) document.
#' @param path_assets Character, the directory to store generated assets.
#'
#' @return A modified tinytable object with images or plots inserted.
#'
#' @details The `plot_tt()` can insert images and inline plots into tables.
#'
#' @examples
#' # HTML output can load images from the web, but not LaTeX
#' # See website for tutorials.
#' fn <- paste0(tempfile(), ".png")
#' dat <- data.frame("R" = "")
#' img <- "https://cran.r-project.org/Rlogo.svg"
#' tt(dat) |>
#'   plot_tt(i = 1, j = 1, path_img = img, height = 7) |>
#'   style_tt(j = 1, align = "c") |>
#'   save_tt(fn)
#'
#' @export
plot_tt <- function(x,
                    i = NULL,
                    j = NULL,
                    height = 1,
                    plot_asp = 1/3,
                    plot_data = NULL,
                    plot_fun = NULL,
                    path_img = NULL,
                    path_assets = "tinytable_assets") {

  assert_integerish(i, null.ok = TRUE)
  assert_integerish(j, null.ok = TRUE)
  assert_numeric(height, len = 1, lower = 0)
  assert_numeric(plot_asp, len = 1, lower = 0, upper = 1)
  assert_class(x, "tinytable")
  out <- x

  ival <- if (is.null(i)) seq_len(meta(x, "nrows")) else i
  jval <- if (is.null(j)) seq_len(meta(x, "ncols")) else j

  len <- length(ival) * length(jval)
  assert_list(plot_data, len = len, null.ok = TRUE)
  assert_list(plot_fun, len = len, null.ok = TRUE)
  assert_character(path_img, len = len, null.ok = TRUE)
  if (!is.null(path_img) && length(path_img) != len) {
    msg <- sprintf("`path_img` must match the dimensions of `i` and `j`: length %s.", len) 
    stop(msg, call. = FALSE)
  }

  # needed when rendering in tempdir()
  out <- meta(out, "path_plot", path_img)

  cal <- call("plot_tt_lazy", 
    i = ival,
    j = jval,
    plot_asp = plot_asp,
    plot_data = plot_data,
    plot_fun = plot_fun,
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
                         height = 1,
                         plot_asp = 1/3,
                         plot_fun = NULL,
                         plot_data = NULL,
                         path_img = NULL,
                         path_assets = "tinytable_assets") {

  out <- x

  if (!is.null(plot_data)) {
    assert_dependency('ggplot2')
    path_img <- NULL
    if (!is.null(meta(x)$output_dir)) {
      path_full <- file.path(meta(x)$output_dir, path_assets)
    }

    if (!dir.exists(path_full)) {
      dir.create(path_full)
    }
    for (idx in seq_along(plot_data)) {
      fn <- paste0(get_id(), ".png")
      fn_full <- file.path(path_full, fn)
      fn <- file.path(path_assets, fn)
      path_img[idx] <- fn
      p <- plot_fun[[idx]](plot_data[[idx]])

      # ggplot2
      if (inherits(p, "ggplot")) {
        assert_dependency("ggplot2")
        suppressMessages(ggplot2::ggsave(
          filename = fn_full,
          width = 1, height = plot_asp,
          units = "in"
        ))

      # base R
      } else if (is.function(p)) {
        png(fn_full, width = 1000, height = 1000 * plot_asp)
        op <- par()
        par(mar = c(0, 0, 0, 0))
        p()
        par(mar = op$mar)
        dev.off()

      # sanity check
      } else {
        msg <- "The functions in the `plot_fun` list must return a function or a `ggplot2` object. See the tutorial online for examples: https://vincentarelbundock.github.io/tinytable"
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



