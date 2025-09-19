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
#' @param fun  String or function to generate inline plots.
#' - Built-in plot types (strings):
#'   - `"histogram"`: Creates histograms from numeric vectors. Accepts `color` argument.
#'   - `"density"`: Creates density plots from numeric vectors. Accepts `color` argument.
#'   - `"bar"`: Creates horizontal bar charts from single numeric values. Accepts `color` and `xlim` arguments.
#'   - `"barpct"`: Creates horizontal percentage bar charts from single numeric values between 0 and 1. Accepts `color` and `background` arguments.
#'   - `"line"`: Creates line plots from data frames with `x` and `y` columns. Accepts `color` and `xlim` arguments.
#' - Custom functions:
#'   - Functions that return `ggplot2` objects.
#'   - Functions that return another function which generates a base `R` plot, ex: `function(x) {function() hist(x)}`
#'   - Note: When using custom ggplot2 functions that return plots with text elements, the text size will normally need to be adjusted because the plot is inserted as a very small image in the table. Text sizes of 1 or smaller often work well (e.g., `theme(text = element_text(size = 1))`).
#' - See the tutorial on the `tinytable` website for more information.
#' @param data a list of data frames or vectors to be used by the plotting functions in `fun`.
#' @param images Character vector, the paths to the images to be inserted. Paths are relative to the main table file or Quarto (Rmarkdown) document.
#' @param assets Path to the directory where generated assets are stored. This path is relative to the location where a table is saved.
#' @param ... Extra arguments are passed to the function in `fun`. Important: Custom plotting functions must always have `...` as an argument.
#'
#' @return A modified tinytable object with images or plots inserted.
#'
#' @details The `plot_tt()` can insert images and inline plots into tables.
#'
#' @examples
#' \dontrun{
#' # Built-in plot types
#' plot_data <- list(mtcars$mpg, mtcars$hp, mtcars$qsec)
#'
#' dat <- data.frame(
#'   Variables = c("mpg", "hp", "qsec"),
#'   Histogram = "",
#'   Density = "",
#'   Bar = "",
#'   BarPct = "",
#'   Line = ""
#' )
#'
#' # Random data for sparklines
#' lines <- lapply(1:3, \(x) data.frame(x = 1:10, y = rnorm(10)))
#'
#' # Percentage data (values between 0 and 1)
#' pct_data <- list(0.65, 0.82, 0.41)
#'
#' tt(dat) |>
#'   plot_tt(j = 2, fun = "histogram", data = plot_data) |>
#'   plot_tt(j = 3, fun = "density", data = plot_data, color = "darkgreen") |>
#'   plot_tt(j = 4, fun = "bar", data = list(2, 3, 6), color = "orange") |>
#'   plot_tt(j = 5, fun = "barpct", data = pct_data, color = "steelblue") |>
#'   plot_tt(j = 6, fun = "line", data = lines, color = "blue") |>
#'   style_tt(j = 2:6, align = "c")
#'
#' # Custom function example (must have ... argument)
#' custom_hist <- function(d, ...) {
#'   function() hist(d, axes = FALSE, ann = FALSE, col = "lightblue")
#' }
#'
#' tt(data.frame(Variables = "mpg", Histogram = "")) |>
#'   plot_tt(j = 2, fun = custom_hist, data = list(mtcars$mpg))
#' }
#'
#' @export
plot_tt <- function(
    x,
    i = NULL,
    j = NULL,
    fun = NULL,
    data = NULL,
    color = "black",
    xlim = NULL,
    height = 1,
    asp = 1 / 3,
    images = NULL,
    assets = "tinytable_assets",
    ...) {
  # non-standard evaluation before anything else
  tmp <- nse_i_j(x, i_expr = substitute(i), j_expr = substitute(j), pf = parent.frame())
  list2env(tmp, environment())

  jval <- sanitize_j(j, x)
  ival <- sanitize_i(i, x, calling_function = "plot_tt")
  assert_numeric(height, len = 1, lower = 0)
  assert_numeric(asp, len = 1, lower = 0, upper = 1)
  assert_class(x, "tinytable")

  # Calculate actual length considering NULL i values
  ival_length <- if (isTRUE(attr(ival, "null"))) {
    length(attr(ival, "body"))
  } else {
    length(ival)
  }
  len <- ival_length * length(jval)

  assert_list(data, len = len, null.ok = TRUE)
  assert_character(images, len = len, null.ok = TRUE)

  if (!is.null(images) && length(images) != len) {
    msg <- sprintf(
      "`images` must match the dimensions of `i` and `j`: length %s.",
      len
    )
    stop(msg, call. = FALSE)
  }

  if (!is.null(fun) && !is.null(images)) {
    stop("`fun` and `images` cannot be used together.", call. = FALSE)
  }

  if (!is.null(fun) && is.null(data)) {
    stop("Please specify `data`.", call. = FALSE)
  }

  if (is.character(fun)) {
    assert_choice(fun, c("histogram", "density", "bar", "barpct", "line"))
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
  } else if (identical(fun, "barpct")) {
    for (idx in seq_along(data)) {
      assert_numeric(data[[idx]], len = 1, name = "data[[1]]")
      if (!all(data[[idx]] >= 0 & data[[idx]] <= 1, na.rm = TRUE)) {
        stop("Data for 'barpct' must be between 0 and 1 (percentages).", call. = FALSE)
      }
    }
    if (is.null(xlim)) {
      xlim <- c(0, 1)
    }
    fun <- rep(list(tiny_barpct), length(data))
  } else {
    fun <- rep(list(fun), length(data))
  }

  # needed when rendering in tempdir()
  cal <- list(
    "plot_tt_lazy",
    x = x,
    i = ival,
    j = jval,
    asp = asp,
    data = data,
    fun = fun,
    color = color,
    xlim = xlim,
    height = height,
    images = images,
    assets = assets
  )
  cal <- c(cal, list(...))
  cal <- do.call(call, cal)

  x@lazy_plot <- c(x@lazy_plot, list(cal))

  return(x)
}

plot_tt_lazy <- function(
    x,
    i = NULL,
    j = NULL,
    height = 1,
    asp = 1 / 3,
    fun = NULL,
    color = NULL,
    data = NULL,
    xlim = NULL,
    images = NULL,
    assets = "tinytable_assets",
    ...) {
  out <- x@data_body

  if (!is.null(data)) {
    assert_dependency("ggplot2")
    images <- NULL

    if (isTRUE(x@output %in% c("html", "bootstrap")) && isTRUE(x@html_portable)) {
      path_full <- tempdir()
      assets <- tempdir()
    } else {
      path_full <- file.path(x@output_dir, assets)
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
        stop(
          "Inline plotting function must have `...` as argument. See tutorial on the `tinytable` website for examples.",
          call. = FALSE
        )
      }
      p <- plot_fun(data[[idx]], xlim = xlim, color = color, ...)

      # ggplot2
      if (inherits(p, "ggplot")) {
        assert_dependency("ggplot2")
        suppressMessages(
          ggplot2::ggsave(
            p,
            filename = fn_full,
            width = 1,
            height = asp,
            units = "in",
            dpi = 1200,
          )
        )

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

  if (isTRUE(x@output == "latex")) {
    cell <- "\\includegraphics[height=%sem]{%s}"
    cell <- sprintf(cell, height, images)
  } else if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator")) && isTRUE(x@html_portable)) {
    assert_dependency("base64enc")

    http <- grepl("^http", trimws(images))
    images[!http] <- encode(images[!http])
    cell <- sprintf('<img src="%s" style="height: %sem;">', images, height)
  } else if (isTRUE(x@output %in% c("html", "bootstrap", "tabulator"))) {
    cell <- ifelse(
      grepl("^http", trimws(images)),
      '<img src="%s" style="height: %sem;">',
      ifelse(
        grepl("^/", trimws(images)) | grepl("^[A-Za-z]:", trimws(images)), # absolute paths (Unix/Windows)
        '<img src="%s" style="height: %sem;">',
        '<img src="./%s" style="height: %sem;">' # relative paths
      )
    )
    cell <- sprintf(cell, images, height)
  } else if (isTRUE(x@output == "markdown")) {
    cell <- "![](%s){ height=%s }"
    cell <- sprintf(cell, images, height * 16)
  } else if (isTRUE(x@output == "typst")) {
    cell <- '#image("%s", height: %sem)'
    cell <- sprintf(cell, images, height)
  } else if (isTRUE(x@output == "dataframe")) {
    cell <- "%s"
    cell <- sprintf(cell, images)
  } else {
    stop("here be dragons")
  }

  # Handle column header insertions (i=0)
  if (0 %in% i) {
    # Insert into header (column names)
    if (is.null(x@names) || length(x@names) == 0) {
      stop("Cannot insert images into header: table has no column names.", call. = FALSE)
    }
    header_indices <- which(i == 0)
    body_indices <- which(i > 0)

    # Insert into column headers
    cell_idx <- 1
    for (idx in header_indices) {
      for (j_val in j) {
        if (j_val <= length(x@names)) {
          x@names[j_val] <- cell[cell_idx]
        }
        cell_idx <- cell_idx + 1
      }
    }

    # Insert into body rows
    if (length(body_indices) > 0) {
      body_i <- i[body_indices]
      for (i_val in body_i) {
        for (j_val in j) {
          out[i_val, j_val] <- cell[cell_idx]
          cell_idx <- cell_idx + 1
        }
      }
    }
  } else {
    # Original behavior: insert into data body
    # Handle the case where i is NA (from sanitize_i when i was NULL)
    if (all(is.na(i)) && isTRUE(attr(i, "null"))) {
      # Use the body rows from the attributes
      i_body <- attr(i, "body")
      out[i_body, j] <- cell
    } else {
      out[i, j] <- cell
    }
  }

  x@data_body <- out

  # Mark columns with HTML content for HTML formatter in Tabulator
  if (isTRUE(x@html_engine == "tabulator")) {
    for (col_idx in j) {
      col_name <- x@names[col_idx]
      if (!is.null(col_name) && !(col_name %in% names(x@tabulator_column_formatters))) {
        x@tabulator_column_formatters[[col_name]] <- list(formatter = "html")
      }
    }
  }

  return(x)
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

tiny_barpct <- function(
    d,
    color = "black",
    background = "lightgrey",
    xlim = c(0, 1),
    ...) {
  function() {
    stopifnot(is.numeric(d), all(d >= 0 & d <= 1, na.rm = TRUE))

    color <- standardize_colors(color)
    bg_col <- standardize_colors(background)

    comp <- 1 - d
    mat <- rbind(d, comp)

    graphics::barplot(
      mat,
      horiz  = TRUE,
      col    = c(color, bg_col),
      xlim   = xlim,
      space  = 0,
      beside = FALSE,
      axes   = FALSE,
      ...
    )
  }
}

tiny_bar <- function(d, color = "black", xlim = 0:1, ...) {
  function() {
    graphics::barplot(d, horiz = TRUE, col = color, xlim = xlim)
  }
}

tiny_line <- function(d, xlim = 0:1, color = "black", ...) {
  function() {
    if (
      !inherits(d, "data.frame") || !"x" %in% names(d) || !"y" %in% names(d)
    ) {
      stop(
        "The data to plot a `line` must be a data frame with columns `x` and `y`.",
        call. = FALSE
      )
    }
    plot(d$x, d$y, type = "l", col = color, axes = FALSE, ann = FALSE, lwd = 50)
  }
}

encode <- function(images) {
  assert_dependency("base64enc")
  ext <- tools::file_ext(images)

  if (any(ext == "")) {
    stop("Empty image extensions are not allowed", call. = FALSE)
  }

  encoded <- sapply(images, base64enc::base64encode)
  sprintf("data:image/%s;base64, %s", ext, encoded)
}
