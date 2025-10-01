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
#' @param height_plot Numeric, the height of generated plot images in pixels (default: 400).
#' @param width_plot Numeric, the width of generated plot images in pixels (default: 1200).
#' @param color string Name of color to use for inline plots (passed to the `col` argument base `graphics` plots in `R`). For bar plots in static output formats (PNG, PDF, etc.), can be a vector of length 2: c(bar_color, background_color) to show progress against a maximum. Note: Tabulator format only uses the first color.
#' @param xlim Numeric vector of length 2. Controls the range of bar plots.
#' @param fun  String or function to generate inline plots.
#' - Built-in plot types (strings):
#'   - `"histogram"`: Creates histograms from numeric vectors. Accepts `color` argument.
#'   - `"density"`: Creates density plots from numeric vectors. Accepts `color` argument.
#'   - `"bar"`: Creates horizontal bar charts from single numeric values. Accepts `color` (single value, or length-2 vector for bar and background colors in static formats) and `xlim` arguments.
#'   - `"line"`: Creates line plots from data frames with `x` and `y` columns. Accepts `color` and `xlim` arguments.
#' - Custom functions:
#'   - Functions that return `ggplot2` objects.
#'   - Functions that return another function which generates a base `R` plot, ex: `function(x) {function() hist(x)}`
#'   - Note: When using custom ggplot2 functions that return plots with text elements, the text size will normally need to be adjusted because the plot is inserted as a very small image in the table. Text sizes of 1 or smaller often work well (e.g., `theme(text = element_text(size = 1))`).
#' - See the tutorial on the `tinytable` website for more information.
#' @param data a list of data frames or vectors to be used by the plotting functions in `fun`.
#' @param images Character vector, the paths to the images to be inserted. Paths are relative to the main table file or Quarto (Rmarkdown) document.
#' @param sprintf Character string, a sprintf format string to format the generated cell content. Default is "%s" which displays the content as-is. Use this to wrap images or plots in custom markup.
#' @param assets Path to the directory where generated assets are stored. This path is relative to the location where a table is saved.
#' @param ... Extra arguments are passed to the function in `fun`. Important: Custom plotting functions must always have `...` as an argument.
#'
#' @return A modified tinytable object with images or plots inserted.
#'
#' @details The `plot_tt()` can insert images and inline plots into tables.
#'
#' @examples
#' \dontrun{
#' # Bar plots with single and dual colors
#' dat <- data.frame(
#'   Metric = c("Sales", "Conversion", "Growth", "Efficiency"),
#'   Value = c(75, 45, 92, 38),
#'   Percentage = c(0.75, 0.45, 0.92, 0.38)
#' )
#'
#' tt(dat) |>
#'   plot_tt(j = 2, fun = "bar", data = as.list(dat$Value), color = "darkorange") |>
#'   plot_tt(j = 3, fun = "bar", data = as.list(dat$Percentage),
#'           color = c("steelblue", "lightgrey"), xlim = c(0, 1))
#'
#' # Built-in plot types
#' plot_data <- list(mtcars$mpg, mtcars$hp, mtcars$qsec)
#'
#' dat <- data.frame(
#'   Variables = c("mpg", "hp", "qsec"),
#'   Histogram = "",
#'   Density = "",
#'   Line = ""
#' )
#'
#' # Random data for sparklines
#' lines <- lapply(1:3, \(x) data.frame(x = 1:10, y = rnorm(10)))
#'
#' tt(dat) |>
#'   plot_tt(j = 2, fun = "histogram", data = plot_data) |>
#'   plot_tt(j = 3, fun = "density", data = plot_data, color = "darkgreen") |>
#'   plot_tt(j = 4, fun = "line", data = lines, color = "blue") |>
#'   style_tt(j = 2:4, align = "c")
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
    height_plot = 400,
    width_plot = 1200,
    images = NULL,
    sprintf = "%s",
    assets = "tinytable_assets",
    ...) {
  # non-standard evaluation before anything else
  tmp <- nse_i_j(x, i_expr = substitute(i), j_expr = substitute(j), pf = parent.frame())
  list2env(tmp, environment())

  jval <- sanitize_j(j, x)
  ival <- sanitize_i(i, x, calling_function = "plot_tt")
  assert_numeric(height, len = 1, lower = 0)
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
    msg <- base::sprintf(
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
    # Always set xlim to c(0, 1) for barpct
    xlim <- c(0, 1)
    # Default to lightgrey background if color is single value
    if (length(color) == 1) {
      color <- c(color, "lightgrey")
    }
    fun <- rep(list(tiny_bar), length(data))
  } else {
    fun <- rep(list(fun), length(data))
  }

  # needed when rendering in tempdir()
  cal <- list(
    "plot_tt_lazy",
    x = x,
    i = ival,
    j = jval,
    data = data,
    fun = fun,
    color = color,
    xlim = xlim,
    height = height,
    height_plot = height_plot,
    width_plot = width_plot,
    images = images,
    sprintf = sprintf,
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
    height_plot = 400,
    width_plot = 1200,
    fun = NULL,
    color = NULL,
    data = NULL,
    xlim = NULL,
    images = NULL,
    sprintf = "%s",
    assets = "tinytable_assets",
    ...) {
  out <- x@data_body

  # Handle Tabulator plots with JavaScript formatters
  # Note: images use the standard HTML path below, which works for tabulator too
  is_tabulator <- isTRUE(x@output == "html" && x@html_engine == "tabulator")
  if (is_tabulator && !is.null(data)) {
    result <- plot_tt_tabulator(
      x, i = i, j = j, fun = fun, data = data,
      color = color, xlim = xlim, ...
    )
    # If plot_tt_tabulator returns NULL, it means we should fall back to PNG rendering
    # (e.g., for custom functions). Otherwise, return the result.
    if (!is.null(result)) {
      return(result)
    }
    # Fall through to PNG rendering below
  }

  is_html <- isTRUE(x@output %in% c("html", "bootstrap", "tabulator"))
  is_quarto <- isTRUE(check_dependency("knitr")) && !is.null(knitr::pandoc_to())

  # paths are tricky in Quarto HTML (website vs single file)
  is_portable <- is_html && (isTRUE(x@html_portable) || is_quarto)
  if (is_portable) assert_dependency("base64enc")

  # Normalize user-provided image paths to full paths
  if (!is.null(images)) {
    # quarto requires relative links or url
    # print("html") must be run from a tempdir on linux, so we need absolute paths
    if (!is_quarto) {
      images <- normalizePath(images, mustWork = FALSE)
    }
  }

  if (!is.null(data)) {
    assert_dependency("ggplot2")
    images <- NULL

    # path_assets directory stores dynamically generated plots
    if (is_portable) {
      path_assets <- tempdir()
      # quarto requires relative paths from the project folder
    } else if (is_quarto) {
      path_assets <- assets
    } else {
      path_assets <- file.path(x@output_dir, assets)
    }
    if (!dir.exists(path_assets)) {
      dir.create(path_assets)
    }

    # Rank hack: prepend zero-padded rank to filename to allow sorting based on
    # file names in interactive tables like tabulator
    last_values <- sapply(data, plot_data_rank)
    ranks <- rank(last_values, ties.method = "first")
    n_digits <- nchar(as.character(length(data)))
    zero_padded_ranks <- sprintf(paste0("%0", n_digits, "d"), ranks)

    for (idx in seq_along(data)) {
      fn <- paste0("tinytable_", zero_padded_ranks[idx], "_", get_id(), ".png")
      fn_full <- file.path(path_assets, fn)
      if (is_portable) {
        # For portable HTML, store the full path for base64 encoding
        images[idx] <- fn_full
      } else {
        # For regular HTML/save_tt/print, store the full path for proper file access
        images[idx] <- fn_full
      }

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
            width = width_plot,
            height = height_plot,
            units = "px"
          )
        )

        # base R
      } else if (is.function(p)) {
        grDevices::png(fn_full, width = width_plot, height = height_plot)
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
    cell <- base::sprintf(cell, height, images)
  } else if (is_portable) {
    http <- grepl("^http", trimws(images))
    images[!http] <- encode(images[!http])
    cell <- base::sprintf('<img src="%s" style="height: %sem;">', images, height)
  } else if (is_html) {
    # Convert relative paths to absolute paths for save_tt/print
    http <- grepl("^http", trimws(images))
    for (img_idx in seq_along(images)) {
      if (!http[img_idx]) {
        # Convert relative paths to absolute paths
        if (!grepl("^/", trimws(images[img_idx])) && !grepl("^[A-Za-z]:", trimws(images[img_idx]))) {
          images[img_idx] <- file.path(x@output_dir, images[img_idx])
        }
      }
    }
    cell <- base::sprintf('<img src="%s" style="height: %sem;">', images, height)
  } else if (isTRUE(x@output == "markdown")) {
    cell <- "![](%s){ height=%s }"
    cell <- base::sprintf(cell, images, height * 16)
  } else if (isTRUE(x@output == "typst")) {
    cell <- '#image("%s", height: %sem)'
    cell <- base::sprintf(cell, images, height)
  } else if (isTRUE(x@output == "dataframe")) {
    cell <- "%s"
    cell <- base::sprintf(cell, images)
  } else {
    stop("here be dragons")
  }

  cell <- base::sprintf(sprintf, cell)

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
        for (j_val in j) {plot_tt
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

plot_data_rank <- function(x) {
  if (is.list(x) || is.data.frame(x)) {
    if (is.data.frame(x) && "y" %in% names(x)) {
      utils::tail(x$y, n = 1)
    } else if (is.list(x)) {
      utils::tail(unlist(x), n = 1)
    } else {
      utils::tail(x, n = 1)
    }
  } else {
    utils::tail(x, n = 1)
  }
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
    if (length(color) == 2) {
      # Two colors: stacked bar with background
      bar_col <- standardize_colors(color[1])
      bg_col <- standardize_colors(color[2])

      # Calculate the remaining portion based on xlim
      max_val <- xlim[2]
      comp <- max_val - d
      mat <- rbind(d, comp)

      graphics::barplot(
        mat,
        horiz  = TRUE,
        col    = c(bar_col, bg_col),
        xlim   = xlim,
        space  = 0,
        beside = FALSE,
        axes   = FALSE,
        ...
      )
    } else {
      # Single color: simple bar without background
      graphics::barplot(d, horiz = TRUE, col = color, xlim = xlim)
    }
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
  base::sprintf("data:image/%s;base64, %s", ext, encoded)
}


#' Handle plot_tt for Tabulator tables
#' @keywords internal
#' @noRd
plot_tt_tabulator <- function(
    x,
    i = NULL,
    j = NULL,
    fun = NULL,
    data = NULL,
    color = "black",
    xlim = NULL,
    ...) {

  # Determine plot type from fun
  plot_type <- NULL
  if (is.list(fun) && length(fun) > 0) {
    # Extract plot type from function name
    fun_obj <- fun[[1]]
    if (identical(fun_obj, tiny_histogram)) {
      plot_type <- "histogram"
    } else if (identical(fun_obj, tiny_density)) {
      plot_type <- "density"
    } else if (identical(fun_obj, tiny_bar)) {
      plot_type <- "bar"
    } else if (identical(fun_obj, tiny_line)) {
      plot_type <- "line"
    }
  }

  if (is.null(plot_type)) {
    # For custom functions, we cannot use JavaScript formatters
    # Signal to continue with standard PNG rendering by returning NULL
    # This will cause plot_tt_lazy to skip the tabulator path and use PNG
    return(NULL)
  }

  # Handle the case where i is NA (from sanitize_i when i was NULL)
  if (all(is.na(i)) && isTRUE(attr(i, "null"))) {
    # Use the body rows from the attributes
    i_body <- attr(i, "body")
  } else {
    i_body <- i
  }

  # Track which custom JS has been marked as needed
  needs_histogram <- FALSE
  needs_sparkline <- FALSE

  # Process each column
  for (col_idx in j) {
    col_name <- x@names[col_idx]

    # Get data for this column
    col_data_idx <- 1
    if (length(j) > 1) {
      col_data_idx <- which(j == col_idx)
    }

    for (row_idx in seq_along(i_body)) {
      data_idx <- (col_data_idx - 1) * length(i_body) + row_idx
      plot_data <- data[[data_idx]]

      # Create formatter configuration
      formatter_info <- tabulator_plot_formatter(
        plot_type = plot_type,
        data = plot_data,
        color = color,
        xlim = xlim
      )

      # Store the formatted data in the cell
      if (plot_type %in% c("line", "density", "histogram")) {
        # For sparkline and histogram, store as JSON array string
        json_array <- paste0("[", paste(formatter_info$data, collapse = ","), "]")
        x@data_body[i_body[row_idx], col_idx] <- json_array
      } else {
        # For progress/bar, store the numeric value
        x@data_body[i_body[row_idx], col_idx] <- formatter_info$data
      }

      # Store the formatter configuration in tabulator_column_formatters (once per column)
      if (is.null(x@tabulator_column_formatters[[col_name]])) {
        x@tabulator_column_formatters[[col_name]] <- formatter_info$config
      }

      # Track which custom JS is needed (once per plot type)
      if (isTRUE(formatter_info$requires_custom_js)) {
        if (plot_type == "histogram") {
          needs_histogram <- TRUE
        } else if (plot_type %in% c("line", "density")) {
          needs_sparkline <- TRUE
        }
      }
    }
  }

  # Add markers for needed custom JS (once total)
  if (needs_histogram && !grepl("NEEDS_HISTOGRAM_JS", x@tabulator_options, fixed = TRUE)) {
    x@tabulator_options <- paste0(x@tabulator_options, "\n// NEEDS_HISTOGRAM_JS")
  }
  if (needs_sparkline && !grepl("NEEDS_SPARKLINE_JS", x@tabulator_options, fixed = TRUE)) {
    x@tabulator_options <- paste0(x@tabulator_options, "\n// NEEDS_SPARKLINE_JS")
  }

  return(x)
}
