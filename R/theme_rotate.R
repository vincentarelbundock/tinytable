rotate_html_table <- function(x, angle) {
  fn <- function(table) {
    table_id <- sprintf("tinytable_%s", table@id)
    rotate_css <- sprintf(
      "transform: rotate(%sdeg); transform-origin: center center; display: inline-block;",
      angle
    )
    pattern <- sprintf('(<[^>]*id="%s"[^>]*style=")([^"]*)(")', table_id)
    if (grepl(pattern, table@table_string, perl = TRUE)) {
      table@table_string <- sub(
        pattern,
        sprintf("\\1\\2 %s\\3", rotate_css),
        table@table_string,
        perl = TRUE
      )
    } else {
      pattern <- sprintf('(<[^>]*id="%s")', table_id)
      table@table_string <- sub(
        pattern,
        sprintf("\\1 style=\"%s\"", rotate_css),
        table@table_string,
        perl = TRUE
      )
    }

    script <- sprintf(
      "<script>window.addEventListener('load', function () { var el = document.getElementById('%s'); if (!el) return; var rect = el.getBoundingClientRect(); var offsetTop = Math.max(0, -rect.top + 10); var offsetLeft = Math.max(0, -rect.left + 10); if (offsetTop > 0) el.style.marginTop = offsetTop + 'px'; if (offsetLeft > 0) el.style.marginLeft = offsetLeft + 'px'; });</script>",
      table_id
    )
    marker <- "<!-- postamble start -->"
    if (grepl(marker, table@table_string, fixed = TRUE)) {
      table@table_string <- sub(
        marker,
        paste(script, marker, sep = "\n"),
        table@table_string,
        fixed = TRUE
      )
    } else {
      table@table_string <- paste(table@table_string, script, sep = "\n")
    }
    return(table)
  }
  build_finalize(x, fn, output = "html")
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

rotate_html_cells <- function(x, angle, i, j) {
  cal <- call(
    "rotate_cells_lazy",
    i = i,
    j = j,
    angle = angle,
    output = "html"
  )
  x@lazy_format <- c(x@lazy_format, list(cal))
  x <- build_finalize(x, add_html_rotate_class, output = "html")
  return(x)
}

rotate_latex_cells <- function(x, angle, i, j) {
  cal <- call(
    "rotate_cells_lazy",
    i = i,
    j = j,
    angle = angle,
    output = "latex"
  )
  x@lazy_format <- c(x@lazy_format, list(cal))
  return(x)
}

rotate_typst_cells <- function(x, angle, i, j) {
  cal <- call(
    "rotate_cells_lazy",
    i = i,
    j = j,
    angle = angle,
    output = "typst"
  )
  x@lazy_format <- c(x@lazy_format, list(cal))
  return(x)
}

rotate_cells_lazy <- function(x, i, j, angle, output) {
  if (inherits(x, "tinytable") && !is.null(output)) {
    if (!identical(x@output, output)) {
      return(x)
    }
  }

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
    components <- "all"
  }

  i <- sanitize_i(i, x, lazy = FALSE, calling_function = "theme_rotate")
  j <- sanitize_j(j, x)

  format_fn <- switch(
    output,
    html = function(vec, ...) {
      # Use writing-mode for 90/270 degrees (proper layout), transform for other angles
      if (angle == 90) {
        ifelse(
          is.na(vec),
          vec,
          sprintf(
            "<span class=\"tinytable-rotate-cell\" style=\"writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap;\">%s</span>",
            vec
          )
        )
      } else if (angle == 270) {
        ifelse(
          is.na(vec),
          vec,
          sprintf(
            "<span class=\"tinytable-rotate-cell\" style=\"writing-mode: vertical-rl; white-space: nowrap;\">%s</span>",
            vec
          )
        )
      } else {
        ifelse(
          is.na(vec),
          vec,
          sprintf(
            "<div class=\"tinytable-rotate-cell\" style=\"display: flex; align-items: center; justify-content: center; width: 100%%; transform: rotate(%sdeg); transform-origin: center center; white-space: nowrap; line-height: 1;\">%s</div>",
            angle,
            vec
          )
        )
      }
    },
    latex = function(vec, ...) {
      ifelse(is.na(vec), vec, sprintf("\\\\rotatebox{%s}{%s}", angle, vec))
    },
    typst = function(vec, ...) {
      ifelse(
        is.na(vec),
        vec,
        sprintf("#rotate(%sdeg, reflow: true, [%s])", -angle, vec)
      )
    }
  )

  x <- apply_format(
    x = x,
    i = i,
    j = j,
    format_fn = format_fn,
    components = components,
    original_data = FALSE
  )

  return(x)
}

add_html_rotate_class <- function(table) {
  table_id <- sprintf("tinytable_%s", table@id)
  # Try pattern where class comes before id
  pattern <- sprintf('(<[^>]*class=")([^"]*)("[^>]*id="%s")', table_id)
  if (grepl(pattern, table@table_string, perl = TRUE)) {
    table@table_string <- sub(
      pattern,
      "\\1\\2 tinytable-rotate-cells\\3",
      table@table_string,
      perl = TRUE
    )
  } else {
    # Try pattern where id comes before class
    pattern <- sprintf('(<[^>]*id="%s"[^>]*class=")([^"]*)(")', table_id)
    if (grepl(pattern, table@table_string, perl = TRUE)) {
      table@table_string <- sub(
        pattern,
        "\\1\\2 tinytable-rotate-cells\\3",
        table@table_string,
        perl = TRUE
      )
    } else {
      # No class attribute found, add one after the id
      pattern <- sprintf('(<[^>]*id="%s")', table_id)
      table@table_string <- sub(
        pattern,
        "\\1 class=\"tinytable-rotate-cells\"",
        table@table_string,
        perl = TRUE
      )
    }
  }
  return(table)
}

#' Rotate table theme (LaTeX, Typst, and HTML)
#'
#' @param x A `tinytable` object.
#' @param angle Numeric. Rotation angle in degrees (0-360).
#' @param i Row indices for rotating cell content. When `i` and `j` are both `NULL`, the
#'   entire table is rotated. In HTML output, rotation is applied inside the cell to keep
#'   borders intact.
#' @param j Column indices for rotating cell content. When `i` and `j` are both `NULL`, the
#'   entire table is rotated. In HTML output, rotation is applied inside the cell to keep
#'   borders intact.
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

  if (is.null(i) && is.null(j)) {
    x <- rotate_latex_table(x, angle)
    x <- rotate_html_table(x, angle)
    x <- rotate_typst_table(x, angle)
    return(x)
  }

  x <- rotate_latex_cells(x, angle, i, j)
  x <- rotate_html_cells(x, angle, i, j)
  x <- rotate_typst_cells(x, angle, i, j)

  return(x)
}
