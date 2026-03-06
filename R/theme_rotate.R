#' Rotate table or cell content
#'
#' In HTML, rotation is always applied to cell content (not the whole table) to
#' keep borders intact. In LaTeX and Typst, when `i` and `j` are both `NULL`,
#' the entire table is rotated; otherwise cell content is rotated.
#'
#' @param x A `tinytable` object.
#' @param angle Numeric. Rotation angle in degrees (0-360).
#' @param i Row indices for cell rotation. When `NULL` (and `j` is also `NULL`),
#'   rotates the entire table in LaTeX/Typst or all cells in HTML.
#' @param j Column indices for cell rotation. When `NULL` (and `i` is also `NULL`),
#'   rotates the entire table in LaTeX/Typst or all cells in HTML.
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_rotate <- function(
  x,
  angle = get_option("tinytable_rotate_angle", default = 90),
  i = NULL,
  j = NULL,
  ...
) {
  assert_numeric(angle, len = 1, lower = 0, upper = 360)

  # LaTeX/Typst: whole-table rotation when no i/j specified
  if (is.null(i) && is.null(j)) {
    x <- rotate_latex_table(x, angle)
    x <- rotate_typst_table(x, angle)
  } else {
    x <- rotate_latex_cells(x, angle, i, j)
    x <- rotate_typst_cells(x, angle, i, j)
  }

  # HTML: always cell-level rotation
  cal <- call(
    "rotate_cells_lazy_html",
    i = i,
    j = j,
    angle = angle
  )
  x@lazy_format <- c(x@lazy_format, list(cal))

  return(x)
}

rotate_latex_table <- function(x, angle) {
  fn <- function(table) {
    rot <- sprintf("\\begin{table}\n\\rotatebox{%s}{", angle)
    table@table_string <- sub(
      "\\begin{table}",
      rot,
      table@table_string,
      fixed = TRUE
    )
    table@table_string <- sub(
      "\\end{table}",
      "}\n\\end{table}",
      table@table_string,
      fixed = TRUE
    )
    return(table)
  }
  build_finalize(x, fn, output = "latex")
}

rotate_typst_table <- function(x, angle) {
  fn <- function(table) {
    rot <- sprintf("#rotate(-%sdeg, reflow: true, [\n  #figure(", angle)
    table@table_string <- sub(
      "#figure(",
      rot,
      table@table_string,
      fixed = TRUE
    )
    table@table_string <- sub(
      ") // end figure",
      ") ]) // end figure",
      table@table_string,
      fixed = TRUE
    )
    return(table)
  }
  build_finalize(x, fn, output = "typst")
}

rotate_latex_cells <- function(x, angle, i, j) {
  cal <- call(
    "rotate_cells_lazy_latex",
    i = i,
    j = j,
    angle = angle
  )
  x@lazy_format <- c(x@lazy_format, list(cal))
  return(x)
}

rotate_typst_cells <- function(x, angle, i, j) {
  cal <- call(
    "rotate_cells_lazy_typst",
    i = i,
    j = j,
    angle = angle
  )
  x@lazy_format <- c(x@lazy_format, list(cal))
  return(x)
}

rotate_cells_setup <- function(x, i, j) {
  if (identical(i, "groupi")) {
    components <- "cells"
    i <- x@group_index_i
  } else if (identical(i, "~groupi")) {
    components <- "cells"
    i <- setdiff(seq_len(nrow(x)), x@group_index_i)
  } else if (is.character(i)) {
    components <- i
    i <- NULL
  } else if (!is.null(i) || !is.null(j)) {
    components <- "cells"
  } else {
    components <- c("colnames", "cells", "groupi", "~groupi", "groupj")
  }
  i <- sanitize_i(i, x, lazy = FALSE, calling_function = "theme_rotate")
  j <- sanitize_j(j, x)
  list(i = i, j = j, components = components)
}

rotate_cells_lazy_html <- function(x, i, j, angle) {
  if (!identical(x@output, "html")) return(x)
  s <- rotate_cells_setup(x, i, j)
  sin_a <- abs(sin(angle * pi / 180))
  format_fn <- if (angle == 90 || angle == 270) {
    transform <- if (angle == 90) "writing-mode: vertical-rl; transform: rotate(180deg);" else "writing-mode: vertical-rl;"
    function(vec, ...) {
      ifelse(
        is.na(vec),
        vec,
        sprintf(
          '<div style="display: inline-block; %s white-space: nowrap;">%s</div>',
          transform,
          vec
        )
      )
    }
  } else {
    function(vec, ...) {
      pad <- sprintf("%.1fem", nchar(vec) * 0.15 * sin_a)
      ifelse(
        is.na(vec),
        vec,
        sprintf(
          '<div style="display: inline-block; transform: rotate(%sdeg); transform-origin: center center; white-space: nowrap; margin: %s 0;">%s</div>',
          angle,
          pad,
          vec
        )
      )
    }
  }
  apply_format(x, i = s$i, j = s$j, format_fn = format_fn, components = s$components, original_data = FALSE)
}

rotate_cells_lazy_latex <- function(x, i, j, angle) {
  if (!identical(x@output, "latex")) return(x)
  s <- rotate_cells_setup(x, i, j)
  format_fn <- function(vec, ...) {
    ifelse(is.na(vec), vec, sprintf("\\rotatebox{%s}{%s}", angle, vec))
  }
  apply_format(x, i = s$i, j = s$j, format_fn = format_fn, components = s$components, original_data = FALSE)
}

rotate_cells_lazy_typst <- function(x, i, j, angle) {
  if (!identical(x@output, "typst")) return(x)
  s <- rotate_cells_setup(x, i, j)
  format_fn <- function(vec, ...) {
    ifelse(
      is.na(vec),
      vec,
      sprintf("#rotate(%sdeg, reflow: true, [%s])", -angle, vec)
    )
  }
  apply_format(x, i = s$i, j = s$j, format_fn = format_fn, components = s$components, original_data = FALSE)
}
