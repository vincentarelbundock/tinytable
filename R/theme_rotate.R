#' Rotate table theme (LaTeX, Typst, and HTML)
#'
#' @param x A `tinytable` object.
#' @param angle Numeric. Rotation angle in degrees (0-360).
#' @param ... Additional arguments (ignored).
#' @return A modified `tinytable` object.
#' @export
theme_rotate <- function(
  x,
  angle = get_option("tinytable_rotate_angle", default = 90),
  ...
) {
  assert_numeric(angle, len = 1, lower = 0, upper = 360)

  # latex
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
  x <- build_finalize(x, fn, output = "latex")

  # html
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
  x <- build_finalize(x, fn, output = "html")

  # typst
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
  x <- build_finalize(x, fn, output = "typst")

  return(x)
}
