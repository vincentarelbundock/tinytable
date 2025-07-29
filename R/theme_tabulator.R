# Helper function to handle CDN theme selection
tabulator_cdn_helper <- function(x, cdn) {
  # Check if it's a custom CDN URL
  if (startsWith(cdn, "http")) {
    css_link <- sprintf('<link href="%s" rel="stylesheet">', cdn)
  } else {
    # Map theme names to CSS filenames
    theme_map <- list(
      default = "tabulator.min.css",
      simple = "tabulator_simple.min.css",
      midnight = "tabulator_midnight.min.css",
      modern = "tabulator_modern.min.css",
      site = "tabulator_site.min.css",
      site_dark = "tabulator_site_dark.min.css",
      bootstrap3 = "tabulator_bootstrap.min.css",
      bootstrap4 = "tabulator_bootstrap4.min.css",
      bootstrap5 = "tabulator_bootstrap5.min.css",
      semanticui = "tabulator_semanticui.min.css",
      bulma = "tabulator_bulma.min.css",
      materialize = "tabulator_materialize.min.css"
    )

    # Validate theme choice using theme_map names
    valid_themes <- names(theme_map)

    if (!cdn %in% valid_themes) {
      warning(
        "Invalid theme '",
        cdn,
        "'. Valid themes are: ",
        paste(valid_themes, collapse = ", "),
        ". Or provide a custom CDN URL starting with 'http'. Using default bootstrap5"
      )
      cdn <- "bootstrap5"
    }

    css_file <- theme_map[[cdn]]
    css_link <- sprintf(
      '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/%s" rel="stylesheet">',
      css_file
    )
  }

  # Replace the CSS link in the table string
  x@table_string <- gsub(
    '<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/tabulator_bootstrap5.min.css" rel="stylesheet">',
    css_link,
    x@table_string,
    fixed = TRUE
  )

  return(x)
}

# Helper function to convert options to JavaScript string
tabulator_options_helper <- function(options = NULL) {
  if (is.null(options) || !is.character(options)) {
    return("")
  }

  # Add comma if options string is not empty
  return(paste0(options, ifelse(nchar(options) > 0, ",", "")))
}


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
    paginationSizeSelector <- sort(pagination)
    selector_str <- paste0("[", paste(paginationSizeSelector, collapse = ", "), "]")
    pagination_opts <- sprintf("
      pagination: 'local',
      paginationSizeSelector: %s,
      paginationSize: %s,", selector_str, paginationSize)
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
