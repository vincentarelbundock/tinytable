#' @section Interactive tables with Tabulator:
#'
#' **Experimental Feature:** The Tabulator.js integration is experimental and the API may change in future versions.
#'
#' The Tabulator.js library provides powerful interactive table features including sorting, filtering, pagination, data export, and real-time editing capabilities. This theme customizes the appearance and behavior of Tabulator tables.
#'
#' **Features:**
#'
#' * Sorting and filtering of all columns
#' * Pagination with configurable page sizes
#' * Search functionality across all columns
#' * Multiple CSS themes and custom styling
#' * Real-time data export options
#' * Accessibility features (ARIA compliant)
#'
#' **Limitations:**
#'
#' * Limited `style_tt()` support (only `align` and `alignv`)
#' * Row-based formatting (`format_tt()` with `i` argument) not supported
#' * Global stylesheets affect all tables in multi-table documents
#' * Date formatting uses Luxon tokens, not R's `strptime` format
#' * Boolean formatting requires `format_tt()` with `bool` argument for custom display

