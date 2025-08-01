switch_latex_environment <- function(table_string, from_env = NULL, to_env = "longtblr", remove_table_wrapper = TRUE) {
  # Define all possible environments
  valid_envs <- c("tblr", "talltblr", "longtblr", "table", "tabular")

  # If from_env is not specified, detect it
  if (is.null(from_env)) {
    for (env in valid_envs) {
      if (grepl(sprintf("\\\\begin\\{%s", env), table_string)) {
        from_env <- env
        break
      }
    }
  }

  # If no environment found, return unchanged
  if (is.null(from_env)) {
    return(table_string)
  }

  # Switch the environment
  table_string <- gsub(
    sprintf("\\\\begin\\{%s", from_env),
    sprintf("\\\\begin\\{%s", to_env),
    table_string
  )
  table_string <- gsub(
    sprintf("\\\\end\\{%s", from_env),
    sprintf("\\\\end\\{%s", to_env),
    table_string
  )

  # Remove table wrapper elements if requested
  if (remove_table_wrapper) {
    table_lines <- strsplit(table_string, "\n")[[1]]
    idx <- grepl(
      "^\\\\caption\\{|^\\\\begin\\{table|^\\\\end\\{table|^\\\\centering",
      trimws(table_lines)
    )
    table_lines <- table_lines[!idx]
    table_string <- paste(table_lines, collapse = "\n")
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
                        ...) {
  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)
  assert_integerish(rowhead, lower = 0, len = 1)
  assert_integerish(rowfoot, lower = 0, len = 1)
  assert_choice(environment, c("tblr", "talltblr", "longtblr"), null.ok = TRUE)

  if (!is.null(inner)) x@tabularray_inner <- c(x@tabularray_inner, inner)
  if (!is.null(outer)) x@tabularray_outer <- c(x@tabularray_outer, outer)

  # environment switch
  if (!is.null(environment) && (rowhead >= 1 || rowfoot >= 1)) {
    stop("When using multipage functionality (rowhead or rowfoot >= 1), the environment must be 'longtblr'.", call. = FALSE)
  }
  fn <- function(table) {
    table@table_string <- switch_latex_environment(
      table@table_string,
      from_env = "tblr",
      to_env = environment,
      remove_table_wrapper = TRUE
    )
    return(table)
  }
  if (!is.null(environment)) {
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
        to_env = "longtblr",
        remove_table_wrapper = TRUE
      )
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }

  return(x)
}
