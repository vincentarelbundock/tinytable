#' Internal HTML Tabulator theme
#' @param x A `tinytable` object.
#' @param tabulator_stylesheet Tabulator CSS theme.
#' @param tabulator_layout Table layout algorithm.
#' @param tabulator_pagination Pagination settings.
#' @param tabulator_search Enable/disable search functionality.
#' @param tabulator_options Custom Tabulator.js configuration options.
#' @param tabulator_css_rule Custom CSS rules.
#' @param tabulator_columns Custom column definitions.
#' @param ... Additional arguments.
#' @keywords internal
#' @noRd
theme_html_tabulator <- function(
    x,
    tabulator_stylesheet = get_option("tinytable_theme_tabulator_stylesheet", default = "bootstrap5"),
    tabulator_layout = get_option(
      "tinytable_theme_tabulator_layout",
      default = "fitDataTable"
    ),
    tabulator_pagination = TRUE,
    tabulator_search = TRUE,
    tabulator_options = get_option("tinytable_theme_tabulator_options", default = NULL),
    tabulator_css_rule = NULL,
    tabulator_columns = NULL,
    ...) {
  assert_choice(
    tabulator_layout,
    choice = c(
      "fitDataTable",
      "fitData",
      "fitDataFill",
      "fitDataStretch",
      "fitColumns"
    )
  )

  # Validate tabulator_css_rule parameter
  if (!is.null(tabulator_css_rule)) {
    if (!is.character(tabulator_css_rule) || length(tabulator_css_rule) != 1) {
      stop("tabulator_css_rule must be a single character string", call. = FALSE)
    }

    if (!grepl("\\$TINYTABLE_ID", tabulator_css_rule)) {
      stop(
        "tabulator_css_rule must contain '$TINYTABLE_ID' placeholder for table scoping.\n",
        "Example: tabulator_css_rule = '$TINYTABLE_ID .tabulator-col { background: red; }'",
        call. = FALSE
      )
    }
  }

  # Validate tabulator_columns parameter
  if (!is.null(tabulator_columns)) {
    if (!is.character(tabulator_columns) || length(tabulator_columns) != 1) {
      stop("tabulator_columns must be a single character string containing valid JavaScript array", call. = FALSE)
    }
  }

  if (isFALSE(tabulator_pagination)) {
    pagination_opts <- "pagination: false,"
  } else if (isTRUE(tabulator_pagination)) {
    if (nrow(x) < 10) {
      pagination_opts <- ""
    } else {
      # Create pagination options: 10, 25, 50, 100, 250 (filtered by available rows)
      tabulator_pagination <- c(10, 25, 50, 100, 250)
      tabulator_pagination <- tabulator_pagination[tabulator_pagination <= nrow(x)]

      # If there are no options above nrow(x), add nrow(x) as an option
      if (max(tabulator_pagination) < nrow(x)) {
        tabulator_pagination <- c(tabulator_pagination, nrow(x))
      }
    }
  }

  if (is.numeric(tabulator_pagination)) {
    # Vector of integers: first is size, sorted vector is selector
    paginationSize <- tabulator_pagination[1]

    # If the number of rows is smaller than the pagination size, adjust pagination size
    if (nrow(x) <= paginationSize) {
      # Use the actual number of rows as pagination size
      paginationSize <- nrow(x)
      tabulator_pagination <- paginationSize
    }

    # Now set up pagination options
    if (length(tabulator_pagination) > 1) {
      # Multiple pagination options: include selector
      paginationSizeSelector <- sort(tabulator_pagination)
      selector_str <- paste0(
        "[",
        paste(paginationSizeSelector, collapse = ", "),
        "]"
      )
      pagination_opts <- sprintf(
        "
        pagination: 'local',
        paginationSizeSelector: %s,
        paginationSize: %s,",
        selector_str,
        paginationSize
      )
    } else {
      # Single pagination option: no selector
      pagination_opts <- sprintf(
        "
        pagination: 'local',
        paginationSize: %s,",
        paginationSize
      )
    }
  }

  # Build layout options
  if (!is.null(x@height)) {
    # Calculate total height: height per row * number of visible rows + header space
    if (is.numeric(tabulator_pagination) && !isFALSE(tabulator_pagination)) {
      # Use pagination size for number of visible rows
      visible_rows <- tabulator_pagination[1]
    } else {
      # Use actual number of rows when no pagination
      visible_rows <- nrow(x)
    }
    total_height <- (x@height * visible_rows) + 2.5 # 2.5em for header
    layout_opts <- sprintf(
      "    layout: '%s',\n    height: '%sem'",
      tabulator_layout,
      total_height
    )
  } else {
    layout_opts <- sprintf("  layout: '%s'", tabulator_layout)
  }

  # Build options string based on whether pagination options exist
  if (nchar(pagination_opts) > 0) {
    opts <- sprintf(
      "    %s
      %s
      ",
      pagination_opts,
      layout_opts
    )
  } else {
    opts <- sprintf(
      "
      %s
      ",
      layout_opts
    )
  }

  # If tabulator_options is provided, use it instead of the individual arguments
  if (!is.null(tabulator_options)) {
    opts <- tabulator_options
  }

  # Only apply to tabulator output
  tabulator_theme_fn <- function(table) {
    if (!isTRUE(table@output == "tabulator")) {
      return(table)
    }

    # Store stylesheet, options, search, css_rule, and columns in S4 slots
    table@tabulator_stylesheet <- tabulator_stylesheet
    table@tabulator_options <- opts
    table@tabulator_search <- tabulator_search
    if (!is.null(tabulator_css_rule)) {
      table@tabulator_css_rule <- tabulator_css_rule
    }
    if (!is.null(tabulator_columns)) {
      table@tabulator_columns <- tabulator_columns
    }

    return(table)
  }
  x <- build_prepare(x, tabulator_theme_fn, output = "tabulator")

  return(x)
}
