theme_tabulator <- function(
    x,
    stylesheet = get_option("tinytable_theme_tabulator_stylesheet", default = "bootstrap5"),
    layout = get_option(
      "tinytable_theme_tabulator_layout",
      default = "fitDataTable"
    ),
    pagination = TRUE,
    search = TRUE,
    options = get_option("tinytable_theme_tabulator_options", default = NULL),
    css_rule = NULL,
    ...) {
  assert_choice(
    layout,
    choice = c(
      "fitDataTable",
      "fitData",
      "fitDataFill",
      "fitDataStretch",
      "fitColumns"
    )
  )

  # Validate css_rule parameter
  if (!is.null(css_rule)) {
    if (!is.character(css_rule) || length(css_rule) != 1) {
      stop("css_rule must be a single character string", call. = FALSE)
    }

    if (!grepl("\\$TINYTABLE_ID", css_rule)) {
      stop(
        "css_rule must contain '$TINYTABLE_ID' placeholder for table scoping.\n",
        "Example: css_rule = '$TINYTABLE_ID .tabulator-col { background: red; }'",
        call. = FALSE
      )
    }
  }

  if (isFALSE(pagination)) {
    pagination_opts <- "pagination: false,"
  } else if (isTRUE(pagination)) {
    if (nrow(x) < 10) {
      pagination_opts <- ""
    } else {
      # Create pagination options: 10, 25, 50, 100, 250 (filtered by available rows)
      pagination <- c(10, 25, 50, 100, 250)
      pagination <- pagination[pagination <= nrow(x)]

      # If there are no options above nrow(x), add nrow(x) as an option
      if (max(pagination) < nrow(x)) {
        pagination <- c(pagination, nrow(x))
      }
    }
  }

  if (is.numeric(pagination)) {
    # Vector of integers: first is size, sorted vector is selector
    paginationSize <- pagination[1]

    # If the number of rows is smaller than the pagination size, adjust pagination size
    if (nrow(x) <= paginationSize) {
      # Use the actual number of rows as pagination size
      paginationSize <- nrow(x)
      pagination <- paginationSize
    }

    # Now set up pagination options
    if (length(pagination) > 1) {
      # Multiple pagination options: include selector
      paginationSizeSelector <- sort(pagination)
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
    if (is.numeric(pagination) && !isFALSE(pagination)) {
      # Use pagination size for number of visible rows
      visible_rows <- pagination[1]
    } else {
      # Use actual number of rows when no pagination
      visible_rows <- nrow(x)
    }
    total_height <- (x@height * visible_rows) + 2.5 # 2.5em for header
    layout_opts <- sprintf(
      "    layout: '%s',\n    height: '%sem'",
      layout,
      total_height
    )
  } else {
    layout_opts <- sprintf("  layout: '%s'", layout)
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

  # If options is provided, use it instead of the individual arguments
  if (!is.null(options)) {
    opts <- options
  }

  # Only apply to tabulator output
  tabulator_theme_fn <- function(table) {
    if (!isTRUE(table@output == "tabulator")) {
      return(table)
    }

    # Store stylesheet, options, search, and css_rule in S4 slots
    table@tabulator_stylesheet <- stylesheet
    table@tabulator_options <- opts
    table@tabulator_search <- search
    if (!is.null(css_rule)) {
      table@tabulator_css_rule <- css_rule
    }

    return(table)
  }

  # Apply the theme function using style_tt's finalize mechanism
  x <- style_tt(x, finalize = tabulator_theme_fn)

  return(x)
}
