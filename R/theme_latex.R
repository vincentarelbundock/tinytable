switch_latex_environment <- function(table_string, table = TRUE, to_env = "longtblr", ncol = NULL) {
  # Define all possible environments
  from_env <- "tblr"
  assert_choice(to_env, c("tblr", "talltblr", "longtblr", "table", "tabular"))

  # If no environment found, return unchanged
  if (is.null(from_env)) {
    return(table_string)
  }

  if (to_env == "longtblr" || isFALSE(table)) {
    remove_table_wrapper <- FALSE
  } else {
    remove_table_wrapper <- TRUE
  }

  # Remove table wrapper elements if requested
  if (remove_table_wrapper) {
    table_string <- lines_drop(table_string, regex = "\\\\begin\\{table\\}", position = "before")
    table_string <- lines_drop(table_string, regex = "\\\\begin\\{table\\}", position = "equal")
    table_string <- lines_drop(table_string, regex = "\\\\end\\{table\\}", position = "after")
    table_string <- lines_drop(table_string, regex = "\\\\end\\{table\\}", position = "equal")
    table_string <- lines_drop(table_string, regex = "\\\\centering", position = "equal")
  }

  # Special handling for tabular environment
  if (to_env == "tabular") {
    # Remove tabularray-specific elements
    table_string <- lines_drop_between(
      table_string,
      regex_start = "tabularray outer open",
      regex_end = "tabularray inner close"
    )
    table_string <- lines_drop(table_string, regex = "tabularray outer close", position = "equal")
    table_string <- lines_drop(table_string, regex = "tabularray inner open", position = "equal")
    table_string <- lines_drop(table_string, regex = "tabularray inner close", position = "equal")
    table_string <- lines_drop(table_string, regex = "^colspec=\\{", position = "equal")

    # Convert tabularray syntax to tabular
    table_string <- gsub("cmidrule\\[(.*?)\\]", "cmidrule(\\1)", table_string)
    table_string <- gsub("\\\\toprule|\\\\midrule|\\\\bottomrule", "\\\\hline", table_string)
    table_string <- sub("\\s*%% tabularray outer open", "", table_string)
    table_string <- sub("\\s*%% TinyTableHeader", "", table_string)
  }

  # Switch the environment
  table_string <- gsub("\\{tblr\\}\\[*", sprintf("{%s}", to_env), table_string)
  table_string <- gsub("\\{talltblr\\}\\[", sprintf("{%s}", to_env), table_string)
  table_string <- gsub("\\{talltblr\\}", sprintf("{%s}", to_env), table_string)
  table_string <- gsub("\\{longtblr\\}\\[", sprintf("{%s}", to_env), table_string)
  table_string <- gsub("\\{longtblr\\}", sprintf("{%s}", to_env), table_string)

  # Set proper column alignment for tabular
  if (to_env == "tabular" && !is.null(ncol)) {
    a <- sprintf("begin{tabular}{%s}", strrep("l", ncol))
    table_string <- sub("begin{tabular}", a, table_string, fixed = TRUE)
  }

  return(table_string)
}


#' LaTeX-specific styles and options
#'
#' @param x A `tinytable` object.
#' @param inner A string that specifies the "inner" settings of a tabularray LaTeX table.
#' @param outer A string that specifies the "outer" settings of a tabularray LaTeX table.
#' @param environment A string that specifies the LaTeX environment to use, default is "tblr".
#' @param rowhead Number of rows to repeat as header on each page. When >= 1, automatically enables multipage mode.
#' @param rowfoot Number of rows to repeat as footer on each page. When >= 1, automatically enables multipage mode.
#' @param multipage Logical. If `TRUE`, enables multipage mode for LaTeX tables (`longtblr` environments).
#' @param table Logical. If `TRUE`, wraps the table in a LaTeX `table` environment. If `FALSE`, uses the `tblr` environment directly. A `table` environment cannot be used with `longtblr` environments.
#' @param resize_width Numeric between 0.01 and 1, representing the proportion of line width for table resizing.
#'   Defaults to `get_option("tinytable_theme_resize_width", 1)`. Only applies when `resize_direction` is specified.
#' @param resize_direction Character string indicating resize direction: "down", "up", or "both".
#'   Defaults to `get_option("tinytable_theme_resize_direction", "down")`. When NULL, no resizing is applied.
#' @param placement String to insert in square brackets after the LaTeX table environment, ex: "H", "htbp".
#'   Defaults to `get_option("tinytable_theme_placement_latex_float", NULL)`. When NULL, no placement is applied.
#' @param ... Additional arguments.
#'
#' @export
theme_latex <- function(x,
                        inner = NULL,
                        outer = NULL,
                        environment = NULL,
                        multipage = FALSE,
                        rowhead = get_option("tinytable_theme_multipage_rowhead", 0L),
                        rowfoot = get_option("tinytable_theme_multipage_rowfoot", 0L),
                        table = TRUE,
                        resize_width = get_option("tinytable_theme_resize_width", 1),
                        resize_direction = NULL,
                        placement = get_option("tinytable_theme_placement_latex_float", NULL),
                        ...) {
  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)
  assert_integerish(rowhead, lower = 0, len = 1)
  assert_integerish(rowfoot, lower = 0, len = 1)
  assert_choice(environment, c("tblr", "talltblr", "longtblr", "tabular"), null.ok = TRUE)
  assert_numeric(resize_width, len = 1, lower = 0.01, upper = 1)
  assert_choice(resize_direction, c("down", "up", "both"), null.ok = TRUE)
  assert_string(placement, null.ok = TRUE)

  if (!is.null(inner)) x@tabularray_inner <- c(x@tabularray_inner, inner)
  if (!is.null(outer)) x@tabularray_outer <- c(x@tabularray_outer, outer)

  # environment switch
  if (!is.null(environment) && (rowhead >= 1 || rowfoot >= 1)) {
    stop("When using multipage functionality (rowhead or rowfoot >= 1), the environment must be 'longtblr'.", call. = FALSE)
  }
  if (!is.null(environment)) {
    fn <- function(table) {
      table@table_string <- switch_latex_environment(
        table@table_string,
        to_env = environment,
        ncol = ncol(table)
      )
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }

  # multipage
  if (isTRUE(multipage)) {
    if (rowhead > 0) {
      x@tabularray_inner <- c(x@tabularray_inner, sprintf("rowhead=%s", rowhead))
    }
    if (rowfoot > 0) {
      x@tabularray_inner <- c(x@tabularray_inner, sprintf("rowfoot=%s", rowfoot))
    }
    fn <- function(table) {
      table@table_string <- switch_latex_environment(
        table@table_string,
        to_env = "longtblr"
      )
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }

  # Handle resize functionality
  if (!is.null(resize_direction)) {
    fn <- function(table) {
      tab <- table@table_string

      if (resize_direction == "both") {
        new <- sprintf("\\resizebox{%s\\linewidth}{!}{", resize_width)
      } else if (resize_direction == "down") {
        new <- sprintf(
          "\\resizebox{\\ifdim\\width>\\linewidth %s\\linewidth\\else\\width\\fi}{!}{",
          resize_width
        )
      } else if (resize_direction == "up") {
        new <- sprintf(
          "\\resizebox{\\ifdim\\width<\\linewidth %s\\linewidth\\else\\width\\fi}{!}{",
          resize_width
        )
      }

      reg <- "\\\\begin\\{tblr\\}|\\\\begin\\{talltblr\\}"
      tab <- lines_insert(tab, regex = reg, new = new, position = "before")

      new <- "}"
      reg <- "\\\\end\\{tblr\\}|\\\\end\\{talltblr\\}"
      tab <- lines_insert(tab, regex = reg, new = new, position = "after")

      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }

  # Handle placement functionality
  if (!is.null(placement)) {
    fn <- function(table) {
      tab <- table@table_string
      tab <- sub(
        "\\\\begin\\{table\\}([^\\[])",
        sprintf("\\\\begin{table}[%s]\\1", placement),
        tab
      )
      table@table_string <- tab
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }

  return(x)
}
