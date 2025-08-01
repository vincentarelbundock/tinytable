# =============================================================================
# TABULATOR HELPER FUNCTIONS
# =============================================================================

#' Helper function to handle CDN theme selection
#' @keywords internal
#' @noRd
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

    assert_dependency("jsonlite")

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
    css_link <- sprintf('<link href="https://cdn.jsdelivr.net/npm/tabulator-tables@6.3/dist/css/%s" rel="stylesheet">', css_file)
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

#' Helper function to convert options to JavaScript string
#' @keywords internal
#' @noRd
tabulator_options_helper <- function(options = NULL) {
  if (is.null(options) || !is.character(options)) {
    return("")
  }

  # Add comma if options string is not empty
  return(paste0(options, ifelse(nchar(options) > 0, ",", "")))
}

#' Apply tabulator formatting to a table
#'
#' @param x A tinytable object
#' @param j Column indices to format
#' @param ... Arguments passed to format_tabulator_column
#' @return Modified tinytable object with tabulator formatters stored
#' @keywords internal
#' @noRd
apply_tabulator_formatting <- function(x, j, ...) {
  if (!is.null(j)) {
    j_sanitized <- sanitize_j(j, x)
    for (col_idx in j_sanitized) {
      formatter_js <- format_tabulator_column(x, j = col_idx, ...)

      # Only store if formatter was created (not NULL for non-formattable columns)
      if (!is.null(formatter_js)) {
        col_name <- x@names[col_idx]
        x@tabulator_column_formatters[[col_name]] <- formatter_js
      }
    }
  }
  return(x)
}

#' Format tabulator column based on data type
#'
#' This function generates Tabulator.js column formatter configurations based on
#' the data type of a column and the same formatting arguments as `format_tt()`.
#' It inspects the data class of column j in x@data and returns appropriate
#' formatter and formatterParams objects for Tabulator.js.
#'
#' @inheritParams format_tt
#' @param x A tinytable object created by `tt()`.
#' @param j Column index or name to format. Must be a single column.
#' @param date A string passed to the `format()` function, such as "%Y-%m-%d". Exception: for tabulator output, this is used directly as the Tabulator.js date format string (e.g., "MM/DD/YYYY"). Note: R and Tabulator.js use different date format specifications.
#'
#' @return A character string containing complete JavaScript column definition
#'   for Tabulator.js including title, field, formatter, and formatterParams
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   price = c(12.34, 56.78, 90.12),
#'   count = c(1L, 2L, 3L),
#'   active = c(TRUE, FALSE, TRUE),
#'   date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
#' )
#' x <- tt(dat, output = "tabulator")
#'
#' # Format numeric column with 3 decimal places
#' format_tabulator_column(x, j = "price", digits = 3)
#'
#' # Format integer column as money
#' format_tabulator_column(x, j = "count", num_mark_big = ",")
#'
#' # Format date column
#' format_tabulator_column(x, j = "date", date = "%m/%d/%Y")
#' }
#'
#' @export
format_tabulator_column <- function(
  x,
  j,
  digits = get_option("tinytable_format_digits", default = NULL),
  num_fmt = get_option("tinytable_format_num_fmt", default = "significant"),
  num_zero = get_option("tinytable_format_num_zero", default = FALSE),
  num_suffix = get_option("tinytable_format_num_suffix", default = FALSE),
  num_mark_big = get_option("tinytable_format_num_mark_big", default = ""),
  num_mark_dec = get_option(
    "tinytable_format_num_mark_dec",
    default = getOption("OutDec", default = ".")
  ),
  date = get_option("tinytable_format_date", default = NULL),
  bool = get_option("tinytable_format_bool", default = NULL),
  math = get_option("tinytable_format_math", default = FALSE),
  other = get_option("tinytable_format_other", default = NULL),
  replace = get_option("tinytable_format_replace", default = FALSE),
  escape = get_option("tinytable_format_escape", default = FALSE),
  markdown = get_option("tinytable_format_markdown", default = FALSE),
  quarto = get_option("tinytable_format_quarto", default = FALSE),
  fn = get_option("tinytable_format_fn", default = NULL),
  sprintf = get_option("tinytable_format_sprintf", default = NULL)
) {
  # Validate inputs
  if (!inherits(x, "tinytable")) {
    stop("`x` must be a tinytable object created by `tt()`.", call. = FALSE)
  }

  if (length(j) != 1) {
    stop("`j` must be a single column index or name.", call. = FALSE)
  }

  # Sanitize column reference
  j_clean <- sanitize_j(j, x)
  if (length(j_clean) != 1) {
    stop("`j` must resolve to a single column.", call. = FALSE)
  }

  # Check that column names exist
  if (is.null(x@names) || length(x@names) == 0) {
    stop(
      "Column names are required for tabulator tables. Use `colnames(x) <- ...` to set column names.",
      call. = FALSE
    )
  }

  # Get the column name and data
  col_name <- x@names[j_clean]
  col_data <- x@data[[j_clean]]
  col_type <- class(col_data)[1]

  # Only apply formatters to numeric, logical, and date columns
  if (
    !col_type %in%
      c("integer", "numeric", "double", "logical", "Date", "POSIXct", "POSIXlt")
  ) {
    return(NULL) # Skip non-formattable columns
  }

  # Create field name (clean for JavaScript)
  field_name <- gsub("\\.", "_", col_name)

  # Initialize formatter configuration
  formatter_config <- list()

  # Determine formatter based on data type and arguments
  if (col_type %in% c("integer", "numeric", "double")) {
    # Numeric formatting
    if (!is.null(sprintf)) {
      # Custom sprintf formatting - use plaintext with custom function
      formatter_config$formatter <- "plaintext"
      # Note: sprintf would need to be handled elsewhere in tabulator
    } else if (!is.null(fn)) {
      # Custom function formatting
      formatter_config$formatter <- "plaintext"
      # Note: custom functions would need to be handled elsewhere
    } else if (isTRUE(num_suffix)) {
      # Number with suffix (K, M, B, T) - use "money" formatter
      formatter_config$formatter <- "money"
      formatter_config$formatterParams <- list(
        decimal = num_mark_dec,
        thousand = num_mark_big,
        precision = if (is.null(digits)) 2 else digits,
        symbol = "",
        symbolAfter = FALSE,
        negativeSign = TRUE,
        suffix = TRUE
      )
    } else {
      # Standard number formatting - use "money" formatter for numeric formatting
      formatter_config$formatter <- "money"

      # Set precision based on digits and num_fmt
      if (col_type == "integer" && is.null(digits)) {
        precision <- 0
      } else if (!is.null(digits)) {
        if (num_fmt == "decimal") {
          precision <- digits
        } else {
          # For significant digits, use digits as precision
          precision <- digits
        }
      } else {
        precision <- if (col_type == "integer") 0 else 2
      }

      formatter_config$formatterParams <- list(
        decimal = num_mark_dec,
        thousand = num_mark_big,
        precision = precision,
        symbol = "", # No currency symbol for plain numbers
        symbolAfter = FALSE,
        negativeSign = TRUE
      )

      # Handle zero padding for decimal format
      if (isTRUE(num_zero) && num_fmt == "decimal") {
        formatter_config$formatterParams$formatEmpty <- TRUE
      }
    }
  } else if (col_type == "logical") {
    # Boolean formatting
    if (!is.null(bool) && is.function(bool)) {
      formatter_config$formatter <- "plaintext"
      # Note: custom bool function would need preprocessing
    } else {
      formatter_config$formatter <- "tickCross"
    }
  } else if (col_type == "Date") {
    # Date formatting - use "datetime" formatter with ISO date string input
    formatter_config$formatter <- "datetime"
    if (!is.null(date)) {
      formatter_config$formatterParams <- list(
        inputFormat = "yyyy-MM-dd",
        outputFormat = date,
        invalidPlaceholder = ""
      )
    } else {
      formatter_config$formatterParams <- list(
        inputFormat = "yyyy-MM-dd",
        outputFormat = "M/d/yyyy",
        invalidPlaceholder = ""
      )
    }
  } else if (col_type %in% c("POSIXct", "POSIXlt")) {
    # DateTime formatting - use "datetime" formatter with timestamp input
    formatter_config$formatter <- "datetime"
    if (!is.null(date)) {
      formatter_config$formatterParams <- list(
        outputFormat = date,
        invalidPlaceholder = ""
      )
    } else {
      formatter_config$formatterParams <- list(
        outputFormat = "M/d/yyyy HH:mm:ss",
        invalidPlaceholder = ""
      )
    }
  }

  # Handle math mode
  if (isTRUE(math)) {
    # For math mode, we'd need to use a custom formatter or HTML formatter
    formatter_config$formatter <- "html"
  }

  # Handle escape - would need preprocessing of data
  if (!isFALSE(escape)) {
    # Escaping would typically be handled in data preprocessing
    # The formatter stays the same but data would be escaped
  }

  # Build complete column definition
  col_def <- list(
    title = col_name,
    field = field_name,
    formatter = formatter_config$formatter
  )

  # Add sorter for datetime columns
  if (formatter_config$formatter == "datetime") {
    col_def$sorter <- "datetime"
    # Add sorterParams to ensure proper date parsing for sorting
    col_def$sorterParams <- list(
      format = "yyyy-MM-dd",
      alignEmptyValues = "bottom"
    )
  }

  # Add formatter parameters if they exist
  if (length(formatter_config$formatterParams) > 0) {
    col_def$formatterParams <- formatter_config$formatterParams
  }

  # Convert to JavaScript string
  js_string <- jsonlite::toJSON(col_def, auto_unbox = TRUE, pretty = FALSE)

  return(js_string)
}
