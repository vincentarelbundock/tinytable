# Line-level access to a tinytable's `@table_string`
#
# `table_string()` rendering proceeds by repeatedly splicing content into the
# template (via lines_insert() / lines_drop() and friends). Each such helper
# historically took and returned a *single* string, so it had to `strsplit()`
# the entire (growing) table on the way in and `paste(collapse)` it on the way
# out - O(N * L) string traffic for N edits on an L-line table. See
# tt_save_audit.md §2.4 / §4.2.
#
# These accessors let a backend hold the table as a character vector of lines
# for the duration of a build phase, apply the cheap `*_vec()` line helpers in
# a loop, and only collapse back to a single string once at the end. The
# resulting output is byte-for-byte identical to the old string round-trips
# because the exact same sequence of edits is applied in the same order - the
# only thing removed is the redundant split/collapse between edits.

#' Get the table string as a character vector of lines
#'
#' @param x A `tinytable` object.
#' @return Character vector (one element per line of `x@table_string`).
#' @keywords internal
#' @noRd
table_string_lines <- function(x) {
  strsplit(x@table_string, "\n", fixed = TRUE)[[1]]
}

#' Set the table string from a character vector of lines
#'
#' Collapses `value` with `"\n"` and assigns it to `x@table_string`.
#'
#' @param x A `tinytable` object.
#' @param value Character vector of lines.
#' @return The modified `tinytable` object (invisibly), as required for an
#'   replacement function.
#' @keywords internal
#' @noRd
`table_string_lines<-` <- function(x, value) {
  x@table_string <- paste(value, collapse = "\n")
  x
}
