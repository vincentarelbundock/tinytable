
theme_tabulator <- function(
    x,
    cdn = get_option("tinytable_theme_tabulator_cdn", default = "bootstrap5"),
    height = 40,
    pagination = c(10, 50, 100, 500),
    layout_width = "fitData",
    layout_height = NULL,
    options = get_option("tinytable_theme_tabulator_options", default = NULL),
    ...) {
  assert_number(height, lower = 1)
  assert_choice(layout_width, choice = c("fitData", "fitDataFill", "fitDataStretch", "fitDataTable", "fitColumns"))
  assert_string(layout_height, null.ok = TRUE)

  if (is.null(pagination)) {
    # No pagination
    pagination_opts <- ""
  } else if (is.numeric(pagination)) {
    # Vector of integers: first is size, sorted vector is selector
    paginationSize <- pagination[1]
    if (length(pagination) > 1) {
      # Multiple pagination options: include selector
      paginationSizeSelector <- sort(pagination)
      selector_str <- paste0("[", paste(paginationSizeSelector, collapse = ", "), "]")
      pagination_opts <- sprintf("
        pagination: 'local',
        paginationSizeSelector: %s,
        paginationSize: %s,", selector_str, paginationSize)
    } else {
      # Single pagination option: no selector
      pagination_opts <- sprintf("
        pagination: 'local',
        paginationSize: %s,", paginationSize)
    }
  }

  # Build layout options
  layout_opts <- sprintf("layout: '%s',", layout_width)
  if (!is.null(layout_height)) {
    layout_opts <- paste0(layout_opts, sprintf("\n    height: '%s',", layout_height))
  }

  opts <- sprintf(
    "
    %s
    %s
    height: '%sem'
    ",
    pagination_opts,
    layout_opts,
    height
  )

  # If options is provided, use it instead of the individual arguments
  if (!is.null(options)) {
    opts <- options
  }

  # Only apply to tabulator output
  tabulator_theme_fn <- function(table) {
    if (!isTRUE(table@output == "tabulator")) {
      return(table)
    }

    # Store CDN and options in S4 slots
    table@tabulator_cdn <- cdn
    table@tabulator_options <- opts


    return(table)
  }

  # Apply the theme function using style_tt's finalize mechanism
  x <- style_tt(x, finalize = tabulator_theme_fn)

  return(x)
}
