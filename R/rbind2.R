#' Combine `tinytable` objects by rows (vertically)
#' 
#' @details
#' `format_tt()` calls applied to `x` or `y` are evaluated before binding, to allow distinct formatting for each panel.
#'
#' Calls to other `tinytable` function such as `style_tt()` or `group_tt()` are ignored when applied to `x` or `y`. These functions should be applied to the final table instead.
#'
#' Information in these S4 slots is carried over from `x` to the combined table:
#'
#' + `x@output`
#' + `x@caption`
#' + `x@width`
#'
#' Information in these S4 slots is concatenated and carried over to the combined table:
#'
#' + `x@notes`
#' + `y@notes`
#'
#' This function requires the `data.table` package.
#'
#' @param x `tinytable` object
#' @param y `tinytable` object
#' @param fill Logical. TRUE fills the missing columns with `NA`
#' @param use.names ‘TRUE’ binds by matching column name, ‘FALSE’ by position
#' @param headers Logical. TRUE inserts the colnames of `y` as an extra row between the two tables.
#' @param ... Additional arguments are ignored.
#' @aliases rbind2
#' @export
setMethod("rbind2", 
          signature = "tinytable", 
          definition = function(x, y, 
                                fill = TRUE, 
                                use.names = TRUE,
                                headers = TRUE,
                                ...) {

  assert_class(x, "tinytable")
  assert_class(y, "tinytable")
  assert_dependency("data.table")
  assert_flag(fill)
  assert_flag(use.names)
  assert_flag(headers)

  x_df <- print(x, output = "dataframe")
  y_df <- print(y, output = "dataframe")

  if (isTRUE(headers) && !is.null(colnames(y))) {
    y_df <- base::rbind(colnames(y_df), y_df)
  }

  out <- data.table::rbindlist(list(x_df, y_df), 
                               fill = fill,
                               use.names = use.names)

  out <- tt(out)

  out@output <- x@output
  out@notes <- c(x@notes, y@notes)
  out@width <- x@width
  out@caption <- x@caption

  return(out)
})
