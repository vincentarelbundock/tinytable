handle_latex_environment <- function(x, environment, environment_table) {
  if (!is.null(environment)) {
    fn <- function(table) {
      table_string <- table@table_string

      # Define all possible environments
      from_env <- "tblr"
      assert_choice(environment, c("tblr", "talltblr", "longtblr", "table", "tabular"))

      # If no environment found, return unchanged
      if (is.null(from_env)) {
        return(table)
      }

      # Switch the environment
      table_string <- gsub("\\{tblr\\}(\\[.*?\\])?", sprintf("{%s}\\1", environment), table_string)
      table_string <- gsub("\\{talltblr\\}(\\[.*?\\])?", sprintf("{%s}\\1", environment), table_string)
      table_string <- gsub("\\{longtblr\\}(\\[.*?\\])?", sprintf("{%s}\\1", environment), table_string)

      # Special handling for tabular environment
      if (environment == "tabular") {
        # insert a duplicate \begin{tabular} without the tblr baggage
        table_string <- lines_insert(table_string,
          new = "\\begin{tabular}",
          regex = "\\\\begin\\{tabular\\}",
          position = "before")
        # tabularray options
        table_string <- lines_drop_between(
          table_string,
          regex_start = "tabularray outer open",
          regex_end = "tabularray inner close"
        )

        # Convert tabularray syntax to tabular
        table_string <- gsub("cmidrule\\[(.*?)\\]", "cmidrule(\\1)", table_string)
        table_string <- gsub("\\\\toprule|\\\\midrule|\\\\bottomrule", "\\\\hline", table_string)
        table_string <- sub("\\s*%% tabularray outer open", "", table_string)
        table_string <- sub("\\s*%% TinyTableHeader", "", table_string)
      }

      # Set proper column alignment for tabular
      if (environment == "tabular" && !is.null(ncol(table))) {
        a <- sprintf("begin{tabular}{%s}", strrep("l", ncol(table)))
        table_string <- sub("begin{tabular}", a, table_string, fixed = TRUE)
      }

      table@table_string <- table_string
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }
  return(x)
}

handle_latex_environment_table <- function(x, environment_table) {
  if (!environment_table) {
    fn <- function(table) {
      # Remove table wrapper elements if requested
      table_string <- table@table_string
      regex <- "\\\\begin\\s*\\{(?:tabular|(?:long|tall)?tblr)\\}"
      table_string <- lines_drop(table_string, regex = regex, position = "before", perl = TRUE)
      table_string <- lines_drop(table_string, regex = "\\\\end\\{table\\}", position = "after")
      table_string <- lines_drop(table_string, regex = "\\\\end\\{table\\}", position = "equal")
      table@table_string <- table_string
      return(table)
    }
    x <- build_finalize(x, fn, output = "latex")
  }
  return(x)
}


#' LaTeX-Specific Theme for `tinytable`
#'
#' This function provides comprehensive LaTeX-specific theming and configuration options
#' for `tinytable` objects. It allows customization of LaTeX environments, table layout,
#' multipage behavior, resizing, and placement within LaTeX documents.
#'
#' @param x A `tinytable` object to apply LaTeX theming to.
#' @param inner Character string specifying inner tabularray options. These options
#'   control the internal formatting of the table (e.g., column alignment, spacing).
#'   Will be added to any existing inner options. Default is `NULL`.
#' @param outer Character string specifying outer tabularray options. These options
#'   control the external formatting around the table. Will be added to any existing
#'   outer options. Default is `NULL`.
#' @param environment Character string specifying the LaTeX table environment to use.
#'   Options are:
#'   - `"tblr"` - Standard tabularray table (default)
#'   - `"talltblr"` - Tall tabularray table for tables that may break across pages
#'   - `"longtblr"` - Long tabularray table for multi-page tables
#'   - `"tabular"` - Basic LaTeX tabular environment without tabularray features
#'
#'   Default is controlled by `tinytable_latex_environment` option.
#' @param environment_table Logical indicating whether to wrap the table in a `table`
#'   environment. When `FALSE`, only the core table structure is output without the
#'   surrounding table wrapper. Automatically set to `FALSE` when `environment = "longtblr"`.
#'   Default is controlled by `tinytable_latex_environment_table` option.
#' @param multipage Logical indicating whether to enable multipage table functionality.
#'   When `TRUE`, automatically switches to `longtblr` environment and sets appropriate
#'   options for tables that span multiple pages. Default is controlled by
#'   `tinytable_latex_multipage` option.
#' @param rowhead Integer specifying the number of header rows to repeat on each page
#'   in multipage tables. Only valid with `longtblr` environment. Default is controlled
#'   by `tinytable_latex_rowhead` option.
#' @param rowfoot Integer specifying the number of footer rows to repeat on each page
#'   in multipage tables. Only valid with `longtblr` environment. Default is controlled
#'   by `tinytable_latex_rowfoot` option.
#' @param resize_width Numeric value between 0.01 and 1.0 specifying the target width
#'   as a fraction of `\\linewidth` when resizing tables. Only used when `resize_direction`
#'   is specified. Default is controlled by `tinytable_latex_resize_width` option.
#' @param resize_direction Character string specifying how to resize tables that are
#'   too wide or too narrow. Options are:
#'   - `"down"` - Only shrink tables wider than `\\linewidth`
#'   - `"up"` - Only expand tables narrower than `\\linewidth`
#'   - `"both"` - Resize all tables to exactly `resize_width * \\linewidth`
#'
#'   Default is controlled by `tinytable_latex_resize_direction` option.
#' @param placement Character string specifying LaTeX float placement options for the
#'   table environment (e.g., "h", "t", "b", "p", "H"). Only used when `environment_table = TRUE`.
#'   Default is controlled by `tinytable_latex_placement` option.
#' @param ... Additional arguments (currently unused).
#'
#' @return A modified `tinytable` object with LaTeX-specific theming applied.
#'
#' @details
#' The function provides fine-grained control over LaTeX table output through several mechanisms:
#'
#' **Environment Selection:**
#' Different LaTeX environments offer different capabilities:
#' - `tblr`: Modern tabularray syntax with full styling support
#' - `talltblr`: Like `tblr` but optimized for tall tables
#' - `longtblr`: Supports page breaks and repeated headers/footers
#' - `tabular`: Basic LaTeX syntax, limited styling but maximum compatibility
#'
#' **Multipage Tables:**
#' When `multipage = TRUE` or when `rowhead`/`rowfoot` are specified, the function
#' automatically switches to `longtblr` environment and disables the table wrapper.
#' This allows tables to break across pages while maintaining headers and footers.
#'
#' **Resizing:**
#' The resize functionality uses LaTeX's `\\resizebox` command to automatically
#' adjust table width based on content and page constraints. This is particularly
#' useful for tables with many columns.
#'
#' **Tabularray Options:**
#' Inner and outer options directly control tabularray formatting. Inner options
#' affect cell content and spacing, while outer options control the table's
#' relationship with surrounding text.
#'
#' @seealso
#' [tt()], [style_tt()], [save_tt()]
#'
#' @export
theme_latex <- function(x,
                        inner = NULL,
                        outer = NULL,
                        environment = get_option("tinytable_latex_environment", default = NULL),
                        environment_table = get_option("tinytable_latex_environment_table", default = TRUE),
                        multipage = get_option("tinytable_latex_multipage", default = FALSE),
                        rowhead = get_option("tinytable_latex_rowhead", 0L),
                        rowfoot = get_option("tinytable_latex_rowfoot", 0L),
                        resize_width = get_option("tinytable_latex_resize_width", 1),
                        resize_direction = get_option("tinytable_latex_resize_direction", default = NULL),
                        placement = get_option("tinytable_latex_placement", NULL),
                        ...) {
  assert_flag(environment_table)
  assert_string(inner, null.ok = TRUE)
  assert_string(outer, null.ok = TRUE)
  assert_integerish(rowhead, lower = 0, len = 1)
  assert_integerish(rowfoot, lower = 0, len = 1)
  assert_choice(environment, c("tblr", "talltblr", "longtblr", "tabular"), null.ok = TRUE)
  assert_numeric(resize_width, len = 1, lower = 0.01, upper = 1)
  assert_choice(resize_direction, c("down", "up", "both"), null.ok = TRUE)
  assert_string(placement, null.ok = TRUE)

  # Set environment_table = FALSE when environment is longtblr
  if (!is.null(environment) && environment == "longtblr") {
    environment_table <- FALSE
  }

  if (!is.null(inner)) x@tabularray_inner <- c(x@tabularray_inner, inner)
  if (!is.null(outer)) x@tabularray_outer <- c(x@tabularray_outer, outer)

  # environment switch
  if (!is.null(environment) && (rowhead >= 1 || rowfoot >= 1)) {
    stop("When using multipage functionality (rowhead or rowfoot >= 1), the environment must be 'longtblr'.", call. = FALSE)
  }

  # Handle environment using separate helper function
  x <- handle_latex_environment(x, environment, environment_table)

  # Handle environment_table using separate helper function
  x <- handle_latex_environment_table(x, environment_table)

  # multipage
  if (isTRUE(multipage)) {
    if (rowhead > 0) {
      x@tabularray_inner <- c(x@tabularray_inner, sprintf("rowhead=%s", rowhead))
    }
    if (rowfoot > 0) {
      x@tabularray_inner <- c(x@tabularray_inner, sprintf("rowfoot=%s", rowfoot))
    }
    # Apply both environment and environment_table changes for multipage
    x <- handle_latex_environment(x, "longtblr", FALSE)
    x <- handle_latex_environment_table(x, FALSE)
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
