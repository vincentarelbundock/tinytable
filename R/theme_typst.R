typst_resize_table <- function(table_string, resize_width, resize_height, resize_direction) {
  if (is.null(resize_direction)) {
    return(table_string)
  }

  if (is.null(resize_height)) {
    target_name <- "target-width"
    target_definition <- sprintf("let target-width = size.width * %s", format_markup_num(resize_width))
    body_dimension <- "body-size.width"
  } else {
    target_name <- "target-height"
    target_definition <- sprintf("let target-height = size.height * %s", format_markup_num(resize_height))
    body_dimension <- "body-size.height"
  }

  condition <- switch(
    resize_direction,
    "down" = sprintf("%s > %s", body_dimension, target_name),
    "up" = sprintf("%s < %s", body_dimension, target_name),
    "both" = "true"
  )

  factor <- sprintf("%s / %s * 100%%", target_name, body_dimension)

  paste0(
    "#layout(size => {\n",
    "  let body = [\n",
    table_string,
    "\n  ]\n",
    "  let body-size = measure(body)\n",
    "  ", target_definition, "\n",
    "  if ", condition, " {\n",
    "    let factor = ", factor, "\n",
    "    scale(x: factor, y: factor, reflow: true, body)\n",
    "  } else {\n",
    "    body\n",
    "  }\n",
    "})"
  )
}

#' Typst-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param multipage Logical. When `TRUE`, emits a Typst show rule that allows figures to break across pages. When `FALSE` (default), tables are kept on a single page. When `NULL`, no show rule is emitted.
#' @param figure Logical, whether to wrap the table in a Typst figure environment and block.
#' @param portable Logical. Sets whether to create portable Typst output with base64-encoded local images embedded directly in the Typst code. Remote image URLs are not downloaded.
#' @param align_figure Character string indicating horizontal alignment: "l", "c", or "r".
#'   Defaults to `get_option("tinytable_typst_align_figure", NULL)`. When NULL, no figure-level alignment is emitted.
#' @param resize_width Numeric value between 0.01 and 1.0 specifying the target width
#'   as a fraction of the available Typst layout width when resize_direction is
#'   specified.
#' @param resize_height Numeric value between 0.01 and 1.0 specifying the target
#'   height as a fraction of the available Typst layout height. When specified,
#'   height controls the scaling and width scales proportionally.
#' @param resize_direction Character string specifying how to resize tables. Options
#'   are: "down" to shrink oversized tables, "up" to expand undersized tables,
#'   and "both" to always scale to the target size.
#' @param ... Additional arguments.
#'
#' @export
theme_typst <- function(x,
                        multipage = get_option("tinytable_typst_multipage", default = FALSE),
                        figure = get_option("tinytable_typst_figure", default = TRUE),
                        portable = get_option("tinytable_typst_portable", default = NULL),
                        align_figure = get_option("tinytable_typst_align_figure", NULL),
                        resize_width = get_option("tinytable_typst_resize_width", 1),
                        resize_height = get_option("tinytable_typst_resize_height", default = NULL),
                        resize_direction = get_option("tinytable_typst_resize_direction", default = NULL), ...) {
  assert_flag(multipage, null.ok = TRUE)
  assert_flag(figure)
  assert_flag(portable, null.ok = TRUE)
  assert_choice(align_figure, c("l", "c", "r"), null.ok = TRUE)
  assert_numeric(resize_width, len = 1, lower = 0.01, upper = 1)
  assert_numeric(resize_height, len = 1, lower = 0.01, upper = 1, null.ok = TRUE)
  assert_choice(resize_direction, c("down", "up", "both"), null.ok = TRUE)

  if (!is.null(portable)) {
    x@typst_portable <- portable
  }

  fn <- function(table) {
    tab <- table@table_string
    if (isTRUE(multipage)) {
      tab <- sub("breakable: false", "breakable: true", tab, fixed = TRUE)
    } else if (isFALSE(multipage)) {
      tab <- sub("breakable: true", "breakable: false", tab, fixed = TRUE)
    } else {
      tab <- lines_drop(tab, regex = "show figure.*breakable")
    }
    table@table_string <- tab
    return(table)
  }
  x <- build_finalize(x, fn, output = "typst")

  if (!figure) {
    fn <- function(table) {
      tab <- table@table_string
      tab <- lines_drop(tab, regex = "table\\(", position = "before")
      tab <- lines_drop(tab, regex = "\\/\\/ end table", position = "after")
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }

  # Handle align_figure functionality
  if (!is.null(align_figure)) {
    fn <- function(table) {
      tab <- table@table_string
      figure_align <- switch(
        align_figure,
        "l" = "left",
        "c" = "center",
        "r" = "right"
      )
      tab <- sub(
        "// tinytable align-figure before",
        sprintf("#align(%s, [", figure_align),
        tab,
        fixed = TRUE
      )
      tab <- sub(
        "// tinytable align-figure after",
        "]) // end align",
        tab,
        fixed = TRUE
      )
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }

  if (!is.null(resize_direction)) {
    fn <- function(table) {
      table@table_string <- typst_resize_table(
        table@table_string,
        resize_width = resize_width,
        resize_height = resize_height,
        resize_direction = resize_direction
      )
      return(table)
    }
    x <- build_finalize(x, fn, output = "typst")
  }

  return(x)
}
